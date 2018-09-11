
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

    method get {q0 s} {
        #: Return the list of tuples selected by an origin state and an input
        #: symbol.
        set items [lsearch -all -index 0 -inline $data $q0]
        return [lsearch -all -index 1 -inline $items $s]
    }

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
        variable limit
        set limit 120
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
        #: TODO update
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

    method MatchTop {source symbol} {
        expr {$symbol eq [lindex $source 0]}
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

    method MatchTape {source symbol} {
        log::log d [info level 0] 
        expr {[lindex $source [lindex $source 0]+1] eq $symbol}
    }

    method MatchAll args {
        return 1
    }

    method PrintMove {tape tokens} {
        log::log d [info level 0] 
        set _tape [lassign $tape tape0]
        lassign $tokens out move
        if {$out eq "E"} {
            lset _tape $tape0 [$ns\::b get]
        } else {
            lset _tape $tape0 $out
        }
        switch $move {
            L {
                incr tape0
                if {$tape0 >= [expr {[llength $_tape] - 1}]} {
                    lappend _tape [$ns\::b get]
                }
            }
            R {
                if {$tape0 < 1} {
                    set _tape [linsert $_tape 0 [$ns\::b get]]
                } else {
                    incr tape0 -1
                }
            }
            N {}
            default {
                error {unexpected alternative}
            }
        }
        log::log d \$tape0=$tape0 
        log::log d \$_tape=$_tape 
        return [linsert $_tape 0 $tape0]
    }

    method NoOp args {
        return {}
    }

    method GetMovesFromTuples tuples {
        set moves {}
        foreach move [lmap tuple $tuples {lrange $tuple 1 end}] {
            dict group moves {*}$move
        }
        return $moves
    }

    method CreateTransitions {varName fnA fnB moves} {
        log::log d [info level 0] 
        upvar 1 $varName _transitions
        set newTuple [list]
        dict for {sym edges} $moves {
            lappend _transitions {*}[lmap edge $edges {
                lassign $edge q1 target
                if {[lindex $fnA 0] eq "PrintMove"} {
                    lset newTuple 0 [my {*}$fnA $target]
                } else {
                    lset newTuple 0 [my {*}$fnA [list $sym]]
                }
                lset newTuple 1 $q1
                lset newTuple 2 [my {*}$fnB $target]
            }]
            log::log d \$_transitions=$_transitions 
        }
    }

    method FilterResults {results select} {
        if {$select eq {}} {
            return $results
        } else {
            return [lselect result {[lindex $result 1] in $select} $results]
        }
    }

    method Inner {transitions matchA methodA methodB {select {}}} {
        log::log d [info level 0] 
        variable limit
        if {[incr limit -1] <= 0} { error {too many iterations} }
        # transitions  = moves into the point(s) where we are now
        # _transitions = moves from that point/those points
        set _transitions [list]
        foreach transition $transitions {
            log::log d \$transition=$transition 
            lassign $transition a q0 b
            # Get possible moves, grouped by input symbol.
            switch $matchA {
                MatchTop {
                    set tuples [concat [my get $q0 {}] [my get $q0 [lindex $a 0]]]
                }
                MatchTape {
                    set tuples [concat [my get $q0 {}] [my get $q0 [lindex $a [lindex $a 0]+1]]]
                }
                MatchAll {
                    set tuples [my get $q0 *]
                }
                default {
                    error $matchA
                }
            }
            set moves [my GetMovesFromTuples $tuples]
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
            return [my Inner $_transitions $matchA $methodA $methodB $select]
        }
    }

}
