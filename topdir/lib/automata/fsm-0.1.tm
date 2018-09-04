
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
package require automata::transitionlist

namespace eval automata {}

oo::class create ::automata::TransitionMatrix {
    variable data is

    constructor args {
        lassign $args data
        set is(epsilon-free) 1
        set is(deterministic) 1
    }

    # TODO this is a deterministic transmat: only one edge per fromState,transSymbol,nextState

    method fromStates {} {
        dict keys $data
    }

    method transSymbols args {
        if {[llength $args] == 0} {
            set _data $data
        } elseif {[llength $args] == 1} {
            lassign $args fromStates
            set _data [dict filter $data script {key val} {
                expr {$key in $fromStates}
            }]
        } else {
            lassign $args fromStates nextStates
            dict for {fromState edges} $data {
                dict for {transSymbol edge} $edges {
                    dict for {nextState target} $edge {
                        if {$fromState in $fromStates && $nextState in $toStates} {
                            lappend result $transSymbol
                        }
                    }
                }
            }
            return $result
        }
        dict for {fromState edges} $_data {
            dict for {transSymbol edge} $edges {
                lappend result $transSymbol
            }
        }
        return $result
    }

    method nextStates {{states {}} {symbols {}}} {
        if {$states eq {}} {
            set _data $data
        } else {
            set _data [dict filter $data script {key val} {
                expr {$key in $states}
            }]
        }
        dict for {fromState edges} $_data {
            if {$symbols eq {}} {
                set _edges $edges
            } else {
                set _edges [dict filter $edges script {key val} {
                    expr {$key in $symbols}
                }]
            }
            dict for {transSymbol edge} $_edges {
                dict for {nextState target} $edge {
                    lappend result $transSymbol
                }
            }
        }
        return $result
    }

    method addTransition {fromState transSymbol nextState {target {}}} {
        dict set data $fromState $transSymbol $nextState $target
    }

    method getEdges fromState {
        dict get $data $fromState
    }

    method getEdge {fromState transSymbol} {
        dict get $data $fromState $transSymbol
    }

}

oo::class create ::automata::FSM {
    mixin ::automata::fa

    variable tuple steps is

    constructor args {
        set is(epsilon-free) 1
        set is(deterministic) 1
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
        my SymbolAAdd $symbol
        foreach target $targets {
            my StateAdd [lindex $target 0]
            foreach sym [lrange $target 1 end] {
                my SymbolBAdd $sym
            }
        }
    }

    method StartingTuples {a b} {
        set a [list {*}$a]
        set b [list {*}$b]
        lmap state [dict get $tuple S] {
            list $a $state $b
        }
    }

    method Moves state {
        if {[dict exists $tuple T $state]} {
            dict get $tuple T $state
        }
    }

    method Consume {source tokens} {
        if {[llength $tokens] == 1 && [lindex $tokens 0] eq {}} {
            return $source
        } else {
            foreach token $tokens {
                if {$token ne [lindex $source 0]} {
                    return -code continue
                }
                set source [lrange $source 1 end]
            }
            return $source
        }
    }

    method Produce {drain tokens} {
        foreach token $tokens {
            if {$token ne {}} {
                lappend drain $token
            }
        }
        return $drain
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
                lassign $stateTuple a q b
                dict for {token targets} [my Moves $q] {
                    set _tapeA [my $methodA $a [list $token]]
                    foreach target $targets {
                        set _tapeB [my $methodB $b [lassign $target q]]
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
        set args [lassign $args a b]
        set stateTuples [my StartingTuples $a $b]
        set results [my Inner {} $stateTuples {*}$args [dict get $tuple F]]
        if {$steps ne {} && $steps > 0} {
            return -code error [format {premature stop with %d steps left} $steps]
        }
        return $results
    }

    method AddState {key state} {
        dict with tuple {
            set Q [lsort -unique [list {*}$Q $state]]
            switch $key {
                Q { }
                S {
                    set S [lsort -unique [list {*}$S $state]]
                    dict set tuple T $state {}
                }
                F {
                    set F [lsort -unique [list {*}$F $state]]
                }
                default {
                    error {unexpected alternative}
                }
            }
        }
        return
    }

    forward StateAdd my AddState Q
    forward StartAdd my AddState S
    forward FinalAdd my AddState F

    method SymbolAAdd symbol {
        if {$symbol ne {}} {
            dict with tuple {
                set A [lsort -unique [list {*}$A $symbol]]
            }
        }
    }

    method SymbolBAdd symbol {
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
        my SymbolAAdd $symbol
        if {![dict exists $tuple T $state $symbol]} {
            dict set tuple T $state $symbol {}
        }
        dict with tuple T $state {
            lappend $symbol $args
        }
        foreach arg [lrange $args 1 end] {
            my SymbolBAdd $arg
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

    method accept a {
        set results [my Iterate $a {} Consume NullTape]
        foreach result $results {
            lassign $result a
            if {[llength $a] == 0} {
                return 1
            }
        }
        return 0
    }

    method recognize {a b} {
        set results [my Iterate $a $b Consume Consume]
        foreach result $results {
            lassign $result a q b
            if {[llength $a] == 0 && [llength $b] == 0} {
                return 1
            }
        }
        return 0
    }

    method translate a {
        set results [my Iterate $a {} Consume Produce]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct b {
        set results [my Iterate {} $b Produce Consume]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        my Iterate -steps $steps {} {} Produce Produce
    }

}
