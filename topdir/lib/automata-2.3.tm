package require json
package require json::write

apply {args {
    set dir [file dirname [info script]]
    foreach arg $args {
        source -encoding utf-8 [file join $dir .. src $arg]
    }
}} set.tcl

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
    variable tuple inputs outputs
    constructor args {
        ::struct::graph G
        lassign $args tuple transitions
        foreach {from edge next} $transitions {
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
                G arc set $a -input $e
                G arc set $a -output $y
            } else {
                return -code error [format {can't build output dictionary from both state and edge}]
            }
        }
    }
    method SetIO pair {
        lassign $pair inputs outputs
        set inputs [list {*}$inputs]
        set outputs [list {*}$outputs]
    }
    method OutputNode vertex {
        if {[G node keyexists $vertex -output]} {
            lappend outputs {*}[G node get $vertex -output]
        }
    }
    method MatchNodeOutput vertex {
        if {[G node keyexists $vertex -output]} {
            set o [G node get $vertex -output]
            set n [llength $o]
            set prefix [lrange $outputs 0 $n-1]
            set outputs [lrange $outputs $n end]
            if {$o ne $prefix} {
                return -code return fail
            }
        }
    }
    method MatchEdgeOutput edge {
        if {[G arc keyexists $edge -output]} {
            set o [G arc get $edge -output]
            set n [llength $o]
            set prefix [lrange $outputs 0 $n-1]
            set outputs [lrange $outputs $n end]
            if {$o ne $prefix} {
                return -code return fail
            }
        }
    }
    method MatchInput edge {
        set i [G arc get $edge -input]
        if {$i ne {}} {
            set n [llength $i]
            set prefix [lrange $inputs 0 $n-1]
            set inputs [lrange $inputs $n end]
            if {$i ne $prefix} {
                return -code return fail
            }
        }
    }
    method OutputEdge edge {
        if {[G arc keyexists $edge -output]} {
            lappend outputs {*}[G arc get $edge -output]
        }
    }
    method PullInputOrBreak {varName arc} {
        upvar 1 $varName n
        set input [G arc get $arc -input]
        if {$input ne {}} {
            incr n -1
            if {$n < 0} {
                return -code break
            }
            lappend inputs $input
        }
    }
    method generate n {
        my SetIO {}
        set current [dict get $tuple s]
        while 1 {
            set arcs [G arcs -out $current]
            # TODO handle #arcs == 0, > 1
            lassign $arcs arc
            set node [G arc target $arc]
            my PullInputOrBreak n $arc
            my OutputNode $node
            my OutputEdge $arc
            set current $node
        }
        list $inputs $outputs
    }
    method recognize args {
        my SetIO $args
        set current [dict get $tuple s]
        while {[llength $inputs] >= 0 && [llength $outputs] > 0} {
            set arcs [G arcs -out $current]
            # TODO handle #arcs == 0, > 1
            lassign $arcs arc
            set node [G arc target $arc]
            my MatchNodeOutput $node
            my MatchInput $arc
            my MatchEdgeOutput $arc
            set current $node
        }
        if {[llength $outputs] > 0} {
            return 0
        }
        expr {$current in [dict get $tuple A]}
    }
    method translate args {
        my SetIO $args
        set current [dict get $tuple s]
        while 1 {
            set arcs [G arcs -out $current]
            # TODO handle #arcs == 0, > 1
            foreach arc $arcs {
                set input [G arc get $arc -input]
                if {$input in [list {} [lindex $inputs 0]]} {
                    break
                }
            }
            if {$input ne {}} {
                if {[llength $inputs] > 0 && $input eq [lindex $inputs 0]} {
                    set inputs [lrange $inputs 1 end]
                } else {
                    break
                }
            }
            set node [G arc target $arc]
            my OutputNode $current
            my OutputEdge $arc
            set current $node
        }
        if {$current in [dict get $tuple A]} {
            return $outputs
        } else {
            return fail
        }
    }
    method reconstruct args {
        my SetIO [linsert $args 0 {}]
        set current [dict get $tuple s]
        while {[llength $outputs] > 0} {
            set arcs [G arcs -out $current]
            # TODO handle #arcs == 0, > 1
            lassign $arcs arc
            set node [G arc target $arc]
            if {$current ne $node} {
                my MatchNodeOutput $node
            }
            lappend inputs {*}[G arc get $arc -input]
            my MatchEdgeOutput $arc
            set current $node
        }
        if {$current in [dict get $tuple A]} {
            return $inputs
        } else {
            return fail
        }
    }
}
