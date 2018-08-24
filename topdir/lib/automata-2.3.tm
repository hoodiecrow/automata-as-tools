package require json
package require json::write

apply {args {
    set dir [file dirname [info script]]
    foreach arg $args {
        source -encoding utf-8 [file join $dir .. src $arg]
    }
}} set.tcl

oo::class create Generator {
    variable m current n tape1 tape2
    constructor args {
        lassign $args m current n tape1 tape2
    }
    method step {} {
        if {$n > 0} {
            foreach arc [$m arcs -out $current] {
                [[self class] new {*}[my GetArgList $arc]] step
            }
        } else {
            $m addResult [list $tape1 $tape2]
        }
        [self] destroy
    }
    method GetArgList edge {
        lappend arglist $m
        lappend arglist [$m arc target $edge]
        lappend arglist [expr {$n - 1}]
        lappend arglist [linsert $tape1 end {*}[$m arc get $edge -input]]
        lappend arglist [linsert $tape2 end {*}[$m arc get $edge -output]]
    }
}

oo::class create Recognizer {
    variable m current tape1 tape2
    constructor args {
        lassign $args m current tape1 tape2
    }
    method step {} {
        set arcs [$m getArcs $current [namespace code [list my Filter]]]
        if {[llength $arcs] > 0 || [llength $tape1] > 0 || [llength $tape2] > 0} {
            foreach arc $arcs {
                [[self class] new {*}[my GetArgList $arc]] step
            }
        } else {
            $m addResult $current
        }
        [self] destroy
    }
    method Filter {g arc} {
        expr {
            ([$g arc get $arc -input] in [list {} [lindex $tape1 0]]) &&
            ([$g arc get $arc -output] in [list {} [lindex $tape2 0]])
        }
    }
    method GetArgList edge {
        lappend arglist $m
        lappend arglist [$m arc target $edge]
        if {[llength [$m arc get $edge -input]] > 0} {
            lappend arglist [lrange $tape1 1 end]
        } else {
            lappend arglist $tape1
        }
        set output [concat [$m edgeGetOutput $edge] [$m nodeGetOutput [$m arc target $edge]]]
        lappend arglist [lrange $tape2 [llength $output] end]
    }
}

oo::class create Translator {
    variable m current tape1 tape2
    constructor args {
        lassign $args m current tape1 tape2
    }
    method step {} {
        set arcs [$m getArcs $current [namespace code [list my Filter]]]
        if {[llength $arcs] > 0 || [llength $tape1] > 0} {
            foreach arc $arcs {
                [[self class] new {*}[my GetArgList $arc]] step
            }
        } else {
            $m addResult $current $tape2
        }
        [self] destroy
    }
    method Filter {g arc} {
        expr {[$g arc get $arc -input] in [list {} [lindex $tape1 0]]}
    }
    method GetArgList edge {
        lappend arglist $m
        lappend arglist [$m arc target $edge]
        if {[llength [$m arc get $edge -input]] > 0} {
            lappend arglist [lrange $tape1 1 end]
        } else {
            lappend arglist $tape1
        }
        # note different output order
        set output [concat [$m nodeGetOutput $current] [$m edgeGetOutput $edge]]
        lappend arglist [linsert $tape2 end {*}$output]
    }
}

oo::class create Reconstructor {
    variable m current tape1 tape2
    constructor args {
        lassign $args m current tape1 tape2
    }
    method step {} {
        set arcs [$m getArcs $current [namespace code [list my Filter]]]
        if {[llength $arcs] > 0 || [llength $tape2] > 0} {
            foreach arc $arcs {
                [[self class] new {*}[my GetArgList $arc]] step
            }
        } else {
            $m addResult $current $tape1
        }
        [self] destroy
    }
    method Filter {g arc} {
        expr {[$g arc get $arc -output] in [list {} [lindex $tape2 0]]}
    }
    method GetArgList edge {
        lappend arglist $m
        lappend arglist [$m arc target $edge]
        # note different input order
        set input [concat [$m nodeGetInput $current] [$m edgeGetInput $edge]]
        lappend arglist [linsert $tape1 end {*}$input]
        if {[llength [$m arc get $edge -output]] > 0} {
            lappend arglist [lrange $tape2 1 end]
        } else {
            lappend arglist $tape2
        }
    }
}

proc tuple2json tuple {
    # tuple keys:
    # A C H I O S T Z: list values
    # b s z:           single values
    # o t:           list of associations
    # A accepting states
    # C control codes ("program")
    # H halting states
    # I input alphabet
    # O output alphabet
    # S state symbols
    # T tape alphabet
    # Z stack symbols
    # b blank tape symbol
    # m 'mark' symbol
    # s starting state
    # z starting stack symbol
    # o output function
    # t transition function
    set result {}
    dict for {k v} $tuple {
        switch $k {
            A - C - H - I - O - S - T -
            Z { dict set result $k [::json::write array {*}[lmap i $v {::json::write string $i}]] }
            b - m - s -
            z { dict set result $k [::json::write string $v] }
            o -
            t {
                set t {}
                dict for {key val} $v {
                    lappend t $key [::json::write array {*}[lmap i $val {::json::write string $i}]]
                }
                dict set result $k [::json::write object {*}$t]
            }
            default {
                return -code error [format {unknown tuple key "%s"} $k]
            }
        }
    }
    return [::json::write object {*}$result]
}

proc assemble items {
    set map {}
    set code {}
    set n 0
    foreach item $items {
        if {[string match *: $item]} {
            lappend map [string trimright $item :] $n
        } elseif {[regexp {^(J[01]?):\*([+-]\d+)$} $item -> op offset]} {
            lappend code $op [expr $n$offset]
        } else {
            lappend code {*}[split $item :]
            set n [llength $code]
        }
    }
    string map $map $code
}

# tuples
# DFSA S I   td    s   A
# NFSA S I   td    s   A
# DFST S I O td od s   A
# NFST S I O td od s   A
# DPDA S I Z td    s z A
#
# *FST modes
# recognizer: accepts when the second tape is a relation of the first
# generator: walk the graph, outputting to both tapes
# translation: creates the second tape according to the first
# rtranslation: creates the first tape according to the second
#
# 1 "a b" 1
# recognize {a a a} {b b b} -> 1
# generate 3 -> {a a a} {b b b}
# translate {a a a} -> {b b b}
# reconstruct {b b b} -> {a a a}
#
# 1 "a a" 2
# 2 "ε a" 1
# recognize {a a} {a a a a} -> 1
# generate 2 -> {a a} {a a a a}
# translate {a a} -> {a a a a}
# reconstruct {a a a a} -> {a a}

# TODO move to better place
package require struct::graph

oo::class create DFST {
    variable tuple result
    constructor args {
        ::struct::graph G
        lassign $args tuple transitions
        foreach {from edge next} $transitions {
            log::log i \$from=[list $from],\ \$edge=[list $edge],\ \$next=[list $next]
            if {[llength $from] > 1 && [llength $edge] == 1} {
                set x [lassign $from s]
                if {![G node exists $s]} {
                    G node insert $s
                }
                G node set $s -output $x
                if {![G node exists $next]} {
                    G node insert $next
                }
                set e $edge
                set a [G arc insert $s $next]
                G arc set $a -input $e
            } elseif {[llength $from] == 1 && [llength $edge] > 1} {
                set s $from
                if {![G node exists $s]} {
                    G node insert $s
                }
                if {![G node exists $next]} {
                    G node insert $next
                }
                set a [G arc insert $s $next]
                set y [lassign $edge e]
                if {$e eq "ε"} {
                    set e {}
                }
                if {$y eq "ε"} {
                    set y {}
                }
                G arc set $a -input $e
                G arc set $a -output $y
            } else {
                return -code error [format {can't build output dictionary from both state and edge}]
            }
        }
    }
    forward arcs G arcs
    forward arc G arc
    method getArcs {current filter} {
        [namespace which G] arcs -out $current -filter $filter
    }
    method nodeGetInput vertex {
        if {[G node keyexists $vertex -input]} {
            G node get $vertex -input
        }
    }
    method edgeGetInput edge {
        if {[G arc keyexists $edge -input]} {
            G arc get $edge -input
        }
    }
    method nodeGetOutput vertex {
        if {[G node keyexists $vertex -output]} {
            G node get $vertex -output
        }
    }
    method edgeGetOutput edge {
        if {[G arc keyexists $edge -output]} {
            G arc get $edge -output
        }
    }
    method addResult args {
        lappend result {*}$args
    }
    method SetTapes pair {
        lassign $pair tape1 tape2
        list [list {*}$tape1] [list {*}$tape2]
    }
    method Accepting accepting {
        foreach state $result {
            if {$state in $accepting} {
                return 1
            }
        }
        return 0
    }
    method FilterResult accepting {
        lmap {state tokens} $result {
            if {$state in $accepting} {
                set tokens
            } else {
                continue
            }
        }
    }
    method generate n {
        set result {}
        [Generator new [self] [dict get $tuple s] $n] step
        return $result
    }
    method recognize args {
        set result {}
        [Recognizer new [self] [dict get $tuple s] {*}[my SetTapes $args]] step
        my Accepting [dict get $tuple A]
    }
    method translate tape {
        set result {}
        [Translator new [self] [dict get $tuple s] [list {*}$tape]] step
        my FilterResult [dict get $tuple A]
    }
    method reconstruct tape {
        set result {}
        [Reconstructor new [self] [dict get $tuple s] {} [list {*}$tape]] step
        my FilterResult [dict get $tuple A]
    }
}
