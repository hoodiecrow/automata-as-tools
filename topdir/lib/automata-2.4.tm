
::tcl::tm::path add [file dirname [file normalize [info script]]]

package require automata::fa

if no {
    apply {args {
            set dir [file dirname [file normalize [info script]]]
            foreach arg $args {
                source -encoding utf-8 [file join $dir .. src $arg]
            }
    }} set.tcl
}

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::fa

    variable tuple T states steps has

    constructor args {
        lassign $args tuple
        set tuple [dict merge {A {} B {} Q {} T {} S {} F {}} $tuple]
        set states {}
        set has(epsilon) 0
        my CompleteTuple
    }

    method CompleteTuple {} {
        dict with tuple {
            dict for {q items} $T {
                lappend Q $q
                dict for {token targets} $items {
                    if {$token eq {}} {
                        set has(epsilon) 1
                    } else {
                        lappend A $token
                    }
                    foreach target $targets {
                        set tokens [lassign $target q]
                        lappend Q $q
                        foreach token $tokens {
                            if {$token ne {}} {
                                lappend B $token
                            }
                        }
                    }
                }
            }
            set A [lsort -unique $A]
            set B [lsort -unique $B]
            set Q [lsort -unique $Q]
            foreach s $S {
                if {$s ni $Q} {
                    return -code error [format {unknown starting state "%s"} $s]
                }
            }
            foreach f $F {
                if {$f ni $Q} {
                    return -code error [format {unknown final state "%s"} $f]
                }
            }
        }
    }

    method StartingTuples {tapeA tapeB} {
        my reset
        set tapeA [list {*}$tapeA]
        set tapeB [list {*}$tapeB]
        lmap state $states {
            list $tapeA $state $tapeB
        }
    }

    method Moves q {
        if {[dict exists $tuple T $q]} {
            dict get $tuple T $q
        }
    }

    method MatchA {varName token tapeA} {
        upvar 1 $varName _tapeA
        if {$token eq {}} {
            set _tapeA $tapeA
            return 1
        } elseif {$token eq [lindex $tapeA 0]} {
            set _tapeA [lrange $tapeA 1 end]
            return 1
        } else {
            return 0
        }
    }

    method MatchB {varName tokens tapeB} {
        upvar 1 $varName _tapeB
        set _tapeB [list]
        if {[llength $tokens] == 1 && [lindex $tokens 0] eq {}} {
            set _tapeB $tapeB
            return 1
        } else {
            foreach token $tokens {
                if {$token ne [lindex $tapeB 0]} {
                    return 0
                }
                set tapeB [lrange $tapeB 1 end]
            }
            set _tapeB $tapeB
            return 1
        }
    }

    method ConsumeInA {tapeA token} {
        if {![my MatchA _tapeA $token $tapeA]} {
            return -code continue
        }
        return $_tapeA
    }

    method ProduceInA {tapeA tokenA} {
        if {$tokenA eq {}} {
            set tapeA
        } else {
            linsert $tapeA end $tokenA
        }
    }

    method ConsumeInB {tapeB target} {
        if {![my MatchB _tapeB $target $tapeB]} {
            return -code continue
        }
        return $_tapeB
    }

    method ProduceInB {tapeB tokens} {
        foreach token $tokens {
            if {$token ne {}} {
                lappend tapeB $token
            }
        }
        return $tapeB
    }

    method NullTape args {
        return {}
    }

    method FilterResults {results select} {
        return [lmap result $results {
            if {$select eq {} || [lindex $result 1] in $select} {
                set result
            } else {
                continue
            }
        }]
    }

    method Inner {results stateTuples methodA methodB {select {}}} {
        if {[llength $stateTuples] == 0} {
            return [my FilterResults $results $select]
        } else {
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                lassign $stateTuple tapeA q tapeB
                dict for {tokenA targets} [my Moves $q] {
                    set _tapeA [my $methodA $tapeA $tokenA]
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
        set results [my Inner {} {*}$args]
        if {$steps ne {} && $steps > 0} {
            return -code error [format {unable to continue with %d steps left} $steps]
        } else {
            return $results
        }
    }

    method reset {} {
        set states [dict get $tuple S]
    }

    method get name {
        dict get $tuple $name
    }

    method accept tape {
        set stateTuples [my StartingTuples $tape {}]
        set results [my Iterate $stateTuples ConsumeInA NullTape [dict get $tuple F]]
        foreach result $results {
            lassign $result tape
            if {[llength $tape] == 0} {
                return 1
            }
        }
        return 0
    }

    method recognize {tapeA tapeB} {
        set stateTuples [my StartingTuples $tapeA $tapeB]
        set results [my Iterate $stateTuples ConsumeInA ConsumeInB [dict get $tuple F]]
        foreach result $results {
            lassign $result tapeA q tapeB
            if {[llength $tapeA] == 0 && [llength $tapeB] == 0} {
                return 1
            }
        }
        return 0
    }

    method translate tapeA {
        set stateTuples [my StartingTuples $tapeA {}]
        set results [my Iterate $stateTuples ConsumeInA ProduceInB [dict get $tuple F]]
        return [lmap result $results {
            lassign $result tapeA q tapeB
            if {[llength $tapeA] == 0} {
                set tapeB
            } else {
                continue
            }
        }]
    }

    method reconstruct tapeB {
        set stateTuples [my StartingTuples {} $tapeB]
        set results [my Iterate $stateTuples ProduceInA ConsumeInA [dict get $tuple F]]
        return [lmap result $results {
            lassign $result tapeA q tapeB
            if {[llength $tapeB] == 0} {
                set tapeA
            } else {
                continue
            }
        }]
    }

    method generate steps {
        set stateTuples [my StartingTuples {} {}]
        my Iterate -steps $steps $stateTuples ProduceInA ProduceInB
    }

}
