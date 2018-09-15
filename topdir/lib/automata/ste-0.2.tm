
namespace eval automata {}

oo::class create ::automata::STE {
    variable data steps newids ns components

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

    method set {q0 syms q1 args} {
        log::log d [info level 0] 
        #: Define a transition. `q0`, `syms`, and `q1` are the origin state, a
        #: list of transition input symbols, and the target state,
        #: respectively. `args` is zero or more symbols that are used for
        #: output or for the stack.  Adding a transition will update the *Q*,
        #: *A*, and *B* set components.
        #: In most cases the list of input symbols will contain one symbol:
        #: passing a list is mostly for compiled transition matrices.
        #: The symbol ε can be used for epsilon moves.
        if {$syms eq "ε"} {
            set syms [list {}]
        }
        foreach sym $syms {
            if {[llength $components] > 0} {
                $ns\::[lindex $components 0] set $q0 $q1
                $ns\::[lindex $components 1] set $sym
                if {[llength $components] > 2 && [llength $args] > 0} {
                    $ns\::[lindex $components 2] set {*}$args
                }
            }
            lappend data [list $q0 $sym $q1 $args]
        }
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

    method fixJumps labels {
        for {set i 0} {$i < [llength $data]} {incr i} {
            lassign [lindex $data $i] q0 - q1
            if {[regexp {^[-+]\d+$} $q1]} {
                lset data $i 2 [expr $q0$q1]
            } elseif {[dict exists $labels $q1]} {
                lset data $i 2 [dict get $labels $q1]
            }
        }
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
        #: Provide arguments for the input sequence, the set of starting
        #: states, the output sequence, and the set of final states.
        #: TODO update
        #: The final arguments are three directives for selecting valid moves
        #: and dealing with the input and output sequence: 1) either MatchTop
        #: for selecting moves by the first input symbol; MatchTape for
        #: selecting by current cell on the tape; or MatchAll to select all
        #: moves. 2) either Consume to remove matching symbols; Produce to add
        #: symbols; Pushdown for stack handling; PrintMove to write to the tape
        #: and move it; or NoOp to skip dealing with the sequence.
        #: TODO update
        set results [my Inner {*}$args]
        if {$steps ne {} && $steps > 0} {
            return -code error [format {premature stop with %d steps left} $steps]
        }
        return $results
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

    method PrintMove {tape tokens} {
        log::log d [info level 0] 
        set _tape [lassign $tape tape0]
        lassign $tokens out move
        if {$out eq "E"} {
            lset _tape $tape0 [$ns\::b get]
        } elseif {$out eq "N"} {
            ;
        } else {
            lset _tape $tape0 $out
        }
        log::log d \$move=$move 
        switch $move {
            R {
                incr tape0
                if {$tape0 >= [expr {[llength $_tape] - 1}]} {
                    lappend _tape [$ns\::b get]
                }
            }
            L {
                if {$tape0 < 1} {
                    set _tape [linsert $_tape 0 [$ns\::b get]]
                } else {
                    incr tape0 -1
                }
            }
            N {}
            default {
                error \$move=$move
            }
        }
        log::log d \$tape0=$tape0 
        log::log d \$_tape=$_tape 
        return [linsert $_tape 0 $tape0]
    }

    method NoOp args {
        return {}
    }

    method GetMoves {matchA q0 a} {
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
        set moves {}
        foreach move [lmap tuple $tuples {lrange $tuple 1 end}] {
            dict group moves {*}$move
        }
        return $moves
    }

    method CreateTransitions {varName fnA fnB moves} {
        upvar 1 $varName _transitions
        set newTuple [list]
        dict for {sym edges} $moves {
            lappend _transitions {*}[lmap edge $edges {
                lassign $edge q1 target
                if {[lindex $fnA 0] in {PrintMove PrintMove2}} {
                    lset newTuple 0 [my {*}$fnA $target]
                } else {
                    lset newTuple 0 [my {*}$fnA [list $sym]]
                }
                lset newTuple 1 $q1
                lset newTuple 2 [my {*}$fnB $target]
            }]
        }
    }

    method Accept id {
        lassign $id a q0
        set tuples [my get $q0 {}]
        set q1s [lmap tuple $tuples {lindex $tuple 2}]
        lappend newids {*}[lmap q1 $q1s {list $a $q1}]
        set tuples [my get $q0 [lindex $a 0]]
        set q1s [lmap tuple $tuples {lindex $tuple 2}]
        if {[llength $q1s] > 0} {
            set a [lrange $a 1 end]
        }
        lappend newids {*}[lmap q1 $q1s {list $a $q1}]
    }

    method Inner {ids matchA methodA methodB} {
        log::log d [info level 0] 
        # ids  = moves into the point(s) where we are now
        # newids = moves from that point/those points
        set newids [list]
        foreach id $ids {
            # Create ids for possible moves.
            my Accept $id
        }
        # Two base cases: 1) no more ids, or 2) steps completed.
        if {
            [llength $newids] == 0 ||
            ($steps ne {} && [incr steps -1] < 0)
        } then {
            return $ids
        } else {
            # Recursive case.
            return [my Inner $newids $matchA $methodA $methodB]
        }
    }

}
