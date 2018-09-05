
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

proc ::tcl::dict::group {varName key args} {
    upvar 1 $varName var
    dict lappend var $key $args
}
namespace ensemble configure dict -map [dict merge [namespace ensemble configure dict -map] {group ::tcl::dict::group}]

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::fa
package require automata::transitionlist

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::fa

    variable tuple steps

    constructor args {
        lassign $args tuple
        set tuple [dict merge {A {} B {} Q {} S {} F {}} $tuple]
        ::automata::TransitionList create T
    }

    forward T T
    export T

    method NormalizeTuple t {
        # TODO depr
        set tuple [dict merge {A {} B {} Q {} S {} F {}} $t]
        dict with tuple {
            set Q [lsort -unique [concat $Q [$T getAllStates]]]
            my AddSymbols A {*}[$T getSymbols]
            my AddSymbols B {*}[$T getAllValueSymbols]
        }
    }

    method StartingTransitions {a b} {
        set a [list {*}$a]
        set b [list {*}$b]
        lmap state [dict get $tuple S] {
            list $a $state $b
        }
    }

    method Consume {source tokens} {
        foreach token [lselect token {$token ne {}} $tokens] {
            if {$token ne [lindex $source 0]} {
                return -code continue
            }
            set source [lrange $source 1 end]
        }
        return $source
    }

    method Produce {drain tokens} {
        lappend drain {*}[lselect token {$token ne {}} $tokens]
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

    method Inner {transitions methodA methodB {select {}}} {
        set _transitions [list]
        foreach transition $transitions {
            lassign $transition a q0 b
            set moves {}
            foreach move [my T getEdges $q0] {
                dict group moves {*}$move
            }
            set newTuple [list]
            dict for {sym edges} $moves {
                lset newTuple 0 [my $methodA $a [list $sym]]
                lappend _transitions {*}[lmap edge $edges {
                    lassign $edge q1 target
                    lset newTuple 1 $q1
                    lset newTuple 2 [my $methodB $b $target]
                }]
            }
        }
        # Two base cases: 1) no more transitions, or 2) steps completed.
        if {
            [llength $_transitions] == 0 ||
            ($steps ne {} && [incr steps -1] < 0)
        } then {
            # Return filtered results in base case.
            return [my FilterResults $transitions $select]
        } else {
            # Recursive case.
            return [my Inner $_transitions $methodA $methodB $select]
        }
    }

    method Iterate args {
        if {[lindex $args 0] eq "-steps"} {
            set args [lassign $args - steps]
        } else {
            set steps {}
        }
        set args [lassign $args a b]
        set transitions [my StartingTransitions $a $b]
        set results [my Inner $transitions {*}$args [dict get $tuple F]]
        if {$steps ne {} && $steps > 0} {
            return -code error [format {premature stop with %d steps left} $steps]
        }
        return $results
    }

    method AddState {key state} {
        dict with tuple {
            if {$key ne "Q"} {
                set Q [lsort -unique [list {*}$Q $state]]
            }
            set $key [lsort -unique [list {*}[set $key] $state]]
        }
        return
    }

    forward StateAdd my AddState Q
    forward StartAdd my AddState S
    forward FinalAdd my AddState F

    method AddSymbols {key args} {
        dict with tuple {
            foreach arg $args {
                if {$arg ne {}} {
                    lappend $key $arg
                }
            }
            set $key [lsort -unique [set $key]]
        }
    }

    method GetTarget {state symbol} {
        my T getTransitions $state $symbol
    }

    method SetTarget {fromstate symbol tostate args} {
        my StateAdd $fromstate
        my AddSymbols A $symbol
        my StateAdd $tostate
        my AddSymbols B {*}$args
        my T add $fromstate $symbol $tostate $args
    }

    method IsEpsilonFree {} {
        my T isEpsilonFree
    }

    method IsDeterministic {} {
        my T isDeterministic
    }

    method IsIn? {key state} {
        dict with tuple {
            set states [set $key]
            expr {([llength $states] == 1 && [lindex $states 0] eq "*") || $state in $states}
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
