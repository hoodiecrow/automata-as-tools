
proc lselect {varName cond items} {
    upvar 1 $varName item
    return [lmap item $items {
        if [uplevel 1 [list expr $cond]] {
            set item
        } else {
            continue
        }
    }]
}

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::fa

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::fa

    variable tuple steps is

    constructor args {
        set is(epsilon-free) 1
        set is(deterministic) 1
        oo::objdefine [self] forward StateAdd my AddState {state {dict set tuple T $state {}}} Q
        oo::objdefine [self] forward StartAdd my AddState {} S
        oo::objdefine [self] forward FinalAdd my AddState {} F
        my NormalizeTuple [lindex $args 0]
    }

    method NormalizeTuple t {
        set tuple [dict merge {A {} B {} Q {} T {} S {} F {}} $t]
        dict with tuple {
            dict for {state transitions} $T {
                lappend Q $state
                dict for {symbol targets} $transitions {
                    my NormalizeTransition $state $symbol $targets
                }
            }
            set A [lsort -unique $A]
            set B [lsort -unique $B]
            set Q [lsort -unique $Q]
        }
    }

    method NormalizeTransition {state symbol targets} {
        if {[llength [dict get $tuple T $state $symbol]] > 1} {
            set is(deterministic) 0
        }
        if {$symbol eq {}} {
            set is(epsilon-free) 0
            set is(deterministic) 0
        }
        my SymbolAdd $symbol
        foreach target $targets {
            my StateAdd [lindex $target 0]
            foreach output [lrange $target 1 end] {
                my OutputAdd $output
            }
        }
    }

    method StartingTuples {tapeA tapeB} {
        set tapeA [list {*}$tapeA]
        set tapeB [list {*}$tapeB]
        lmap state [dict get $tuple S] {
            list $tapeA $state $tapeB
        }
    }

    method Moves state {
        if {[dict exists $tuple T $state]} {
            dict get $tuple T $state
        }
    }

    method Consume {tape tokens} {
        if {[llength $tokens] == 1 && [lindex $tokens 0] eq {}} {
            return $tape
        } else {
            foreach token $tokens {
                if {$token ne [lindex $tape 0]} {
                    return -code continue
                }
                set tape [lrange $tape 1 end]
            }
            set return $tape
        }
    }

    method Produce {tape tokens} {
        foreach token $tokens {
            if {$token ne {}} {
                lappend tape $token
            }
        }
        return $tape
    }

    method NullTape args {
        return {}
    }

    method FilterResults {results select} {
        if {$select eq {}} {
            return $results
        } else {
            return [lselect result {[lindex $result 1] in $select} $results]
        }
    }

    method Inner {results stateTuples methodA methodB {select {}}} {
        # Two base cases: 1) no more transitions, or 2) steps completed.
        # Return filtered results in base case.
        if {[llength $stateTuples] == 0} {
            return [my FilterResults $results $select]
        } else {
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                lassign $stateTuple tapeA q tapeB
                dict for {token targets} [my Moves $q] {
                    set _tapeA [my $methodA $tapeA [list $token]]
                    foreach target $targets {
                        set _tapeB [my $methodB $tapeB [lassign $target q]]
                        lappend _stateTuples [list $_tapeA $q $_tapeB]
                    }
                }
            }
            if {$steps ne {}} {
                if {[incr steps -1] < 0} {
                    return [my FilterResults $stateTuples $select]
                }
            }
            return [my Inner $stateTuples $_stateTuples $methodA $methodB $select]
        }
    }

    method Iterate args {
        if {[lindex $args 0] eq "-steps"} {
            set args [lassign $args - steps]
        } else {
            set steps {}
        }
        set args [lassign $args tapeA tapeB]
        set stateTuples [my StartingTuples $tapeA $tapeB]
        set results [my Inner {} $stateTuples {*}$args [dict get $tuple F]]
        if {$steps ne {} && $steps > 0} {
            return -code error [format {premature stop with %d steps left} $steps]
        }
        return $results
    }

    method AddState {ifmissing key state} {
        dict with tuple {
            if {$state ni $Q} {
                if {$ifmissing eq {}} {
                    return -code error [format {state "%s" not in state set} $state]
                } else {
                    apply [linsert $ifmissing end [self namespace]] $state
                }
            }
            set $key [lsort -unique [list {*}[set $key] $state]]
        }
        return
    }

    method SymbolAdd symbol {
        if {$symbol ne {}} {
            dict with tuple {
                set A [lsort -unique [list {*}$A $symbol]]
            }
        }
    }

    method OutputAdd symbol {
        log::log d [info level 0] 
        if {$symbol ne {}} {
            dict with tuple {
                set B [lsort -unique [list {*}$B $symbol]]
            }
        }
    }

    method SymbolsFrom from {
        if {[dict exists $tuple T $from]} {
            lmap {token -} [dict get $tuple T $from] {set token}
        }
    }

    method SymbolsFromTo {from to} {
        set result [list]
        if {[dict exists $tuple T $from]} {
            dict for {token targets} [dict get $tuple T $from] {
                foreach target $targets {
                    if {[lindex $target 0] eq $to} {
                        lappend result $token
                    }
                }
            }
        }
        return $result
    }

    method GetTarget {state symbol} {
        dict get $tuple T $state $symbol
    }

    method SetTarget {state symbol args} {
        my StateAdd $state
        my SymbolAdd $symbol
        if {![dict exists $tuple T $state $symbol]} {
            dict set tuple T $state $symbol {}
        }
        dict with tuple T $state {
            lappend $symbol $args
        }
        foreach output [lrange $args 1 end] {
            my OutputAdd $output
        }
        my NormalizeTransition $state $symbol [lrange $args 0 0]
    }

    method IsIn? {key state} {
        dict with tuple {
            set states [set $key]
            expr {[llength $states] == 0 || $state in $states}
        }
    }

    method IsInMultiple? {key stateset} {
        dict with tuple {
            set states [set $key]
            foreach state $stateset {
                if {$state in $states} {
                    return 1
                }
            }
            return 0
        }
    }

    method get args {
        # assume that things exist
        dict get $tuple {*}$args
    }

    method symbols {} {
        lsort -unique [concat [dict get $tuple A] [dict get $tuple B]]
    }

    method accept tape {
        set results [my Iterate $tape {} Consume NullTape]
        foreach result $results {
            lassign $result tape
            if {[llength $tape] == 0} {
                return 1
            }
        }
        return 0
    }

    method recognize {tapeA tapeB} {
        set results [my Iterate $tapeA $tapeB Consume Consume]
        foreach result $results {
            lassign $result tapeA q tapeB
            if {[llength $tapeA] == 0 && [llength $tapeB] == 0} {
                return 1
            }
        }
        return 0
    }

    method translate tape {
        set results [my Iterate $tape {} Consume Produce]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct tape {
        set results [my Iterate {} $tape Produce Consume]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        my Iterate -steps $steps {} {} Produce Produce
    }

}
