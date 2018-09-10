
namespace eval automata {}

oo::class create ::automata::STE {
    variable data steps ns components

    # "State Transition Engine"
    #
    # Handles the definition of transition relations and iteration through the
    # transition matrix.
    #
    # Supports finite state automata and pushdown automata. Use TME for
    # Turing-class machines.

    constructor args {
        #: Use arguments to set the namespace of the machine tuple, and the
        #: names of the components affected by defining a transition.
        lassign $args ns components
    }

    method Dump {} {set data}

    method print {} {
        #: Print the component's transitions.
        lappend res "Transitions"
        lappend res [format {%-3s %-3s %-3s %s} q0 inp q1 out]
        foreach t $data {
            lassign $t q0 inp q1 out
            if {$inp eq {}} {
                set inp ε
            }
            lappend res [format {%-3s %-3s %-3s %s} $q0 $inp $q1 $out]
        }
        return [join $res \n]
    }

    method isEpsilonFree {} {
        #: Is the transition matrix free from epsilon transitions?
        expr {{} ni [my getSymbols]}
    }

    method isDeterministic {} {
        #: Is the transition matrix deterministic?
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

    method set {q0 sym q1 args} {
        #: Define a transition. `q0`, `sym`, and `q1` are the origin state, the
        #: transition input symbol, and the target state, respectively. `args`
        #: is zero or more symbols that are used for output or for the stack.
        #: Adding a transition will update the *Q*, *A*, and *B* set
        #: components.
        if {$sym eq "ε"} {
            set sym {}
        }
        $ns\::[lindex $components 0] set $q0 $q1
        $ns\::[lindex $components 1] set $sym
        if {[llength $args] > 0} {
            $ns\::[lindex $components 2] set {*}$args
        }
        lappend data [list $q0 $sym $q1 $args]
    }

    method getAllStates {} {
        #: Return the set of all states in the transition matrix.
        lsort -unique [concat \
            [lmap item $data {lindex $item 0}] \
            [lmap item $data {lindex $item 2}]]
    }

    method getFromStates {} {
        #: Return the set of all origin states in the transition matrix.
        lsort -unique [lmap item $data {lindex $item 0}]
    }

    method getSymbols {{q0 *} {q1 *}} {
        #: Return the set of input symbols in the transition matrix. The set
        #: can be limited to specific origin and/or target state.
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 2 -inline $items $q1]
        lmap item $items {lindex $item 1}
    }

    method getEdges q0 {
        #: Return the list of edges from a given origin state.
        set items [lsearch -all -index 0 -inline $data $q0]
        lmap item $items {lrange $item 1 end}
    }

    method getTransitions {q0 s} {
        #: Return the list of transitions selected by an origin state and an
        #: input symbol.
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 1 -inline $items $s]
        lmap item $items {lrange $item 2 end}
    }

    forward get my getTransitions
        #: `get` is a synonym for `getTransitions`.

    method getTargets {q0 s {q1 *}} {
        #: Return the output values for given origin state, input symbol, and
        #: optionally target state.
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 1 -inline $items $s]
        set items [lsearch -all -index 2 -inline $items $q1]
        lmap item $items {lindex $item end}
    }

    method getValues {} {
        #: Return all output values (groups of output symbols).
        lmap item $data {lindex $item end}
    }

    method getAllValueSymbols {} {
        #: Return the set of all output symbols.
        lsort -unique [concat {*}[my getValues]]
    }

    method iterate args {
        #: Start a walk through the transition matrix.
        log::log d [info level 0] 
        if {[lindex $args 0] eq "-steps"} {
            #: The option `-steps steps` is recognized: if given it limits the
            #: number of steps the walk will comprise.
            set args [lassign $args - steps]
        } else {
            set steps {}
        }
        set args [lassign $args a s b f]
        #: Provide arguments for the input sequence, the set of starting
        #: states, the output sequence, and the set of final states.
        set transitions [my StartingTransitions $a $s $b]
        set results [my Inner $transitions {*}$args $f]
        #: The final arguments are two directives for dealing with the input
        #: and output sequence: either `Consume` for matching and removing
        #: symbols; `Produce` for rule-based addition of symbols; `Pushdown`
        #: for stack handling; or `NoOp` to avoid dealing with the sequence.
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
        log::log d [info level 0] 
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

    method Pushdown {stack tokens} {
        log::log d [info level 0] 
        set tokens [lassign $tokens top]
        if {$top ne [lindex $stack 0]} {
            return -code continue
        }
        lreplace $stack 0 0 {*}[lselect token {$token ne {}} $tokens]
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
            log::log d \$transition=$transition 
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
