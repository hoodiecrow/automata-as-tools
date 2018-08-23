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
    variable tuple tape1 tape2 result
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
    method SetTapes pair {
        lassign $pair tape1 tape2
        set tape1 [list {*}$tape1]
        set tape2 [list {*}$tape2]
    }
    method GetInputFromEdge arc {
        G arc get $arc -input
    }
    method GetInputFromTape {} {
        set tape1 [lassign $tape1 input]
        return $input
    }
    method GetOutputFromNode vertex {
        if {[G node keyexists $vertex -output]} {
            G node get $vertex -output
        }
    }
    method GetOutputFromEdge edge {
        if {[G arc keyexists $edge -output]} {
            G arc get $edge -output
        }
    }
    method OutputNode vertex {
        if {[G node keyexists $vertex -output]} {
            lappend tape2 {*}[G node get $vertex -output]
        }
    }
    method OutputEdge edge {
        if {[G arc keyexists $edge -output]} {
            lappend tape2 {*}[G arc get $edge -output]
        }
    }
    method MatchNodeOutput2 vertex {
        if {[G node keyexists $vertex -output]} {
            set o [G node get $vertex -output]
            set n [llength $o]
            set prefix [lrange $tape2 0 $n-1]
            set tape2 [lrange $tape2 $n end]
            if {$o ne $prefix} {
                return 0
            }
        }
        return 1
    }
    method MatchNodeOutput vertex {
        if {[G node keyexists $vertex -output]} {
            set o [G node get $vertex -output]
            set n [llength $o]
            set prefix [lrange $tape2 0 $n-1]
            set tape2 [lrange $tape2 $n end]
            if {$o ne $prefix} {
                return -code return fail
            }
        }
    }
    method MatchEdgeOutput2 edge {
        if {[G arc keyexists $edge -output]} {
            set o [G arc get $edge -output]
            set n [llength $o]
            set prefix [lrange $tape2 0 $n-1]
            set tape2 [lrange $tape2 $n end]
            if {$o ne $prefix} {
                return 0
            }
        }
        return 1
    }
    method MatchEdgeOutput edge {
        if {[G arc keyexists $edge -output]} {
            set o [G arc get $edge -output]
            set n [llength $o]
            set prefix [lrange $tape2 0 $n-1]
            set tape2 [lrange $tape2 $n end]
            if {$o ne $prefix} {
                return -code return fail
            }
        }
    }
    method MatchInput2 {varName edge} {
        upvar 1 $varName is
        foreach i0 [my GetInputFromEdge $edge] {
            set is [lassign $is i1]
            if {$i0 ne $i1} {
                return 0
            }
        }
        return 1
    }
    method MatchInput edge {
        foreach i0 [my GetInputFromEdge $edge] {
            set i1 [my GetInputFromTape]
            if {$i0 ne $i1} {
                return 0
            }
        }
        return 1
    }
    method MatchOutput2 {varName node edge} {
        upvar 1 $varName os
        foreach o0 [concat [my GetOutputFromNode $node] [my GetOutputFromEdge $edge]] {
            set os [lassign $os o1]
            if {$o0 ne $o1} {
                return 0
            }
        }
        return 1
    }
    method Accepting {} {
        foreach state $result {
            if {$state in [dict get $tuple A]} {
                return 1
            }
        }
        return 0
    }
    method FilterResult {} {
        lmap {state tokens} $result {
            if {$state in [dict get $tuple A]} {
                set tokens
            } else {
                continue
            }
        }
    }
    method Rgenerate {current n {is {}} {os {}}} {
        foreach arc [G arcs -out $current] {
            set input [my GetInputFromEdge $arc]
            if {$input ne {}} {
                if {[incr n -1] < 0} {
                    lappend result [list $is $os]
                    continue
                }
            }
            set node [G arc target $arc]
            set output [concat [my GetOutputFromEdge $arc] [my GetOutputFromNode $node]]
            my Rgenerate $node $n [linsert $is end {*}$input] [linsert $os end {*}$output]
        }
    }
    method generate n {
        set result {}
        my Rgenerate [dict get $tuple s] $n
        return $result
    }
    method GetEpsilons {_ arc} {
        my GetMatchInput {} G $arc
    }
    method GetMatchInput {token _ arc} {
        string equal [G arc get $arc -input] $token
    }
    method MatchOutput3 {output oti} {
        if {$output eq {}} {
            return 1
        }
        set n [expr {$oti + [llength $output] - 1}]
        foreach o0 $output o1 [lrange $tape2 $oti $n] {
            if {$o0 ne $o1} {
                return 0
            }
        }
        return 1
    }
    method MatchOutput4 {arc oti} {
        set node [G arc target $arc]
        set output [concat [my GetOutputFromEdge $arc] [my GetOutputFromNode $node]]
        if {$output eq {}} {
            return $oti
        }
        set n [expr {$oti + [llength $output] - 1}]
        foreach o0 $output o1 [lrange $tape2 $oti $n] {
            if {$o0 ne $o1} {
                return -1
            }
        }
        return [incr n]
    }
    method Rrecognize {current {t0i 0} {t1i 0}} {
        set input {}
        set arcs [G arcs -out $current -filter [namespace code [list my GetMatchInput $input]]]
        foreach arc $arcs {
            set _t1i [my MatchOutput4 $arc $t1i]
            if {$_t1i >= 0} {
                my Rrecognize [G arc target $arc] $t0i $_t1i
            }
        }
        if {$t0i < [llength $tape1]} {
            set input [lindex $tape1 $t0i]
            set _t0i [expr {$t0i + 1}]
            set arcs [G arcs -out $current -filter [namespace code [list my GetMatchInput $input]]] 
            if {[llength $arcs] > 0} {
                if {$_t0i >= [llength $tape1]} {
                    # do final series of ε moves
                    set input {}
                    set arcs [G arcs -out $current -filter [namespace code [list my GetMatchInput $input]]]
                    while {[llength $arcs] > 0} {
                        foreach arc $arcs {
                            set _t1i [my MatchOutput4 $arc $t1i]
                            if {$_t1i >= 0} {
                                # TODO assumes only one epsilon move
                                set current [G arc target $arc]
                            }
                        }
                        set arcs [G arcs -out $current -filter [namespace code [list my GetMatchInput $input]]]
                    }
                    lappend result $current
                } else {
                    foreach arc $arcs {
                        set _t1i [my MatchOutput4 $arc $t1i]
                        if {$_t1i >= 0} {
                            my Rrecognize [G arc target $arc] $_t0i $_t1i
                        }
                    }
                }
            }
        }
    }
    method recognize args {
        my SetTapes $args
        set result {}
        my Rrecognize [dict get $tuple s]
        my Accepting
    }
    method Rtranslate {current is {os {}}} {
        foreach arc [G arcs -out $current] {
            set node [G arc target $arc]
            set input [my GetInputFromEdge $arc]
            if {$input ne {} && [llength $is] == 0} {
                lappend result $current $os
                break
            } else {
                # note different output order
                set output [concat [my GetOutputFromNode $current] [my GetOutputFromEdge $arc]]
                set nis $is
                if {[my MatchInput2 nis $arc]} {
                    my Rtranslate $node $nis [linsert $os end {*}$output]
                }
            }
        }
    }
    method translate args {
        my SetTapes $args
        set result {}
        my Rtranslate [dict get $tuple s] $tape1
        my FilterResult
    }
    method Rreconstruct {current is os} {
        foreach arc [G arcs -out $current] {
            set node [G arc target $arc]
            set input [my GetInputFromEdge $arc]
            set nos $os
            if {![my MatchOutput2 nos $current $arc]} {
                continue
            }
            if {[llength $nos] == 0} {
                lappend result $node $is
                break
            } else {
                my Rreconstruct $node [linsert $is end {*}$input] $nos
            }
        }
    }
    method reconstruct args {
        my SetTapes [linsert $args 0 {}]
        set result {}
        my Rreconstruct [dict get $tuple s] $tape1 $tape2
        my FilterResult
    }
}
