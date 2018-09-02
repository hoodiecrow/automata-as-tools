
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::fa

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::fa

    variable tuple steps has

    constructor args {
        set has(epsilon) 0
        my NormalizeTuple [lindex $args 0]
    }

    method NormalizeTuple t {
        set tuple [dict merge {A {} B {} Q {} T {} S {} F {}} $t]
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
        set tapeA [list {*}$tapeA]
        set tapeB [list {*}$tapeB]
        lmap state [dict get $tuple S] {
            list $tapeA $state $tapeB
        }
    }

    method Moves q {
        if {[dict exists $tuple T $q]} {
            dict get $tuple T $q
        }
    }

    method ConsumeInA {tapeA token} {
        if {$token eq {}} {
            set _tapeA $tapeA
        } elseif {$token eq [lindex $tapeA 0]} {
            set _tapeA [lrange $tapeA 1 end]
        } else {
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

    method ConsumeInB {tapeB tokens} {
        set _tapeB [list]
        if {[llength $tokens] == 1 && [lindex $tokens 0] eq {}} {
            set _tapeB $tapeB
        } else {
            foreach token $tokens {
                if {$token ne [lindex $tapeB 0]} {
                    return -code continue
                }
                set tapeB [lrange $tapeB 1 end]
            }
            set _tapeB $tapeB
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

    method AddState s {
        dict with tuple {
            if {![dict exists $T $s]} {
                dict set T $s {}
            }
            set Q [lsort -unique [list {*}$Q $s]]
        }
    }

    method AddStart state {
        dict with tuple {
            if {$state ni $Q} {
                return -code error [format {state "%s" not in state set} $state]
            }
            set S [lsort -unique [list {*}$S $state]]
        }
    }

    method AddFinal state {
        dict with tuple {
            if {$state ni $Q} {
                return -code error [format {state "%s" not in state set} $state]
            }
            set F [lsort -unique [list {*}$F $state]]
        }
    }

    method AddSymbol symbol {
        if {$symbol ne {}} {
            dict with tuple {
                set A [lsort -unique [list {*}$A $symbol]]
            }
        }
    }

    method AddOutput symbol {
        if {$symbol ne {}} {
            dict with tuple {
                set B [lsort -unique [list {*}$B $symbol]]
            }
        }
    }

    method SymbolsS state {
        if {[dict exists $tuple T $state]} {
            lmap {token -} [dict get $tuple T $state] {set token}
        }
    }

    method SymbolsT {state destination} {
        set result [list]
        if {[dict exists $tuple T $state]} {
            dict for {token targets} [dict get $tuple T $state] {
                foreach target $targets {
                    if {[lindex $target 0] eq $destination} {
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

    method SetTarget {state symbol "-->" target} {
        my AddState $state
        my AddSymbol $symbol
        if {![dict exists $tuple T $state $symbol]} {
            dict set tuple T $state $symbol {}
        }
        dict with tuple T $state {
            lappend $symbol $target
        }
        foreach output [lrange $target 1 end] {
            my AddOutput $output
        }
        # TODO overkill
        if no {
            my NormalizeTuple $tuple
        }
    }

    method get name {
        dict get $tuple $name
    }

    method edges state {
        if {[dict exists $tuple T $state]} {
            dict get $tuple T $state
        }
    }

    method states {} {
        dict get $tuple Q
    }

    method state? state {
        expr {$state in [dict get $tuple Q]}
    }

    method startstates {} {
        dict get $tuple S
    }

    method start? state {
        dict with tuple {
            expr {$S eq {} || $state in $S}
        }
    }

    method finalstates {} {
        dict get $tuple F
    }

    method final? state {
        dict with tuple {
            expr {$F eq {} || $state in $F}
        }
    }

    method symbols {} {
        lsort -unique [concat [dict get $tuple A] [dict get $tuple B]]
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
        set results [my Iterate $stateTuples ProduceInA ConsumeInB [dict get $tuple F]]
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
