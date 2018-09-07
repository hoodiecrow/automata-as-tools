
namespace eval automata {}

# "State Transition Engine"
oo::class create ::automata::STE {
    variable data steps

    constructor args {
        lassign $args data
        set is(epsilon-free) 1
        set is(deterministic) 1
    }

    method dump {} {set data}

    method isEpsilonFree {} {
        expr {{} ni [my getSymbols]}
    }

    method isDeterministic {} {
        if {![my isEpsilonFree]} {
            return 0
        } else {
            foreach tuple $data {
                if {[incr freq([lrange $tuple 0 1])] > 1} {
                    return 0
                }
            }
            return 1
        }
    }

    method add {q0 sym q1 {v {}}} {
        if {$sym eq "ε"} {
            set sym {}
        }
        lappend data [list $q0 $sym $q1 $v]
    }

    method getAllStates {} {
        lsort -unique [concat \
            [lmap item $data {lindex $item 0}] \
            [lmap item $data {lindex $item 2}]]
    }

    method getFromStates {} {
        lsort -unique [lmap item $data {lindex $item 0}]
    }

    method getSymbols {{q0 *} {q1 *}} {
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 2 -inline $items $q1]
        lmap item $items {lindex $item 1}
    }

    method getEdges q0 {
        set items [lsearch -all -index 0 -inline $data $q0]
        lmap item $items {lrange $item 1 end}
    }

    method getTransitions {q0 s} {
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 1 -inline $items $s]
        lmap item $items {lrange $item 2 end}
    }

    method getTargets {q0 s {q1 *}} {
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 1 -inline $items $s]
        set items [lsearch -all -index 2 -inline $items $q1]
        lmap item $items {lindex $item end}
    }

    method getValues {} {
        lmap item $data {lindex $item end}
    }

    method getAllValueSymbols {} {
        lsort -unique [concat {*}[my getValues]]
    }

    # ---

    method iterate args {
        log::log d [info level 0] 
        if {[lindex $args 0] eq "-steps"} {
            set args [lassign $args - steps]
        } else {
            set steps {}
        }
        set args [lassign $args a s b f]
        set transitions [my StartingTransitions $a $s $b]
        set results [my Inner $transitions {*}$args $f]
        if {$steps ne {} && $steps > 0} {
            return -code error [format {premature stop with %d steps left} $steps]
        }
        return $results
    }

    method StartingTransitions {a states b} {
        set a [list {*}$a]
        set b [list {*}$b]
        lmap state $states {
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

    method NoOp args {
        return {}
    }

    method GetMoves q0 {
        set moves {}
        foreach move [my getEdges $q0] {
            dict group moves {*}$move
        }
        return $moves
    }

    method CreateTransitions {varName fnA fnB moves} {
        upvar 1 $varName _transitions
        set newTuple [list]
        dict for {sym edges} $moves {
            lset newTuple 0 [my {*}$fnA [list $sym]]
            lappend _transitions {*}[lmap edge $edges {
                lassign $edge q1 target
                lset newTuple 1 $q1
                lset newTuple 2 [my {*}$fnB $target]
            }]
        }
    }

    method FilterResults {results select} {
        if {$select eq {}} {
            return $results
        } else {
            return [lselect result {[lindex $result 1] in $select} $results]
        }
    }

    method Inner {transitions methodA methodB {select {}}} {
        # transitions  = moves into the point(s) where we are now
        # _transitions = moves from that point/those points
        set _transitions [list]
        foreach transition $transitions {
            lassign $transition a q0 b
            # Get possible moves, grouped by input symbol.
            set moves [my GetMoves $q0]
            # Create transitions for possible moves.
            my CreateTransitions _transitions [list $methodA $a] [list $methodB $b] $moves
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

}
