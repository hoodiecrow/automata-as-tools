
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
        lassign $args components
        set ns [namespace qualifiers [self]]
        set name [namespace tail [self]]
        lappend $ns\::complist $name
        oo::objdefine [uplevel 1 {self}] forward $name $name
        oo::objdefine [uplevel 1 {self}] export $name
        oo::objdefine [self] forward Q $ns\::Q
    }

    method Dump {} {set data}

    method print {} {
        #: Print the component's transitions.
        lappend res "Transitions"
        lappend res [format {%-5s %-5s %-5s %s} q0 inp q1 out]
        foreach t $data {
            set out [lassign $t q0 inp q1]
            if {$inp eq {}} {
                set inp ε
            }
            lappend res [format {%-5s %-5s %-5s %s} $q0 $inp $q1 $out]
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
        log::log i [info level 0] 
        #: Define a transition. `q0`, `syms`, and `q1` are the origin state, a
        #: list of transition input symbols, and the target state,
        #: respectively. `args` is zero or more symbols that are used for
        #: output or for the stack.  Adding a transition will update the *Q*,
        #: *A*, and *B* set components.
        #: In most cases the list of input symbols will contain one symbol:
        #: passing a list is mostly for compiled transition matrices.
        foreach sym $syms {
            if {[llength $components] > 0} {
                $ns\::[lindex $components 0] set $q0 $q1
                $ns\::[lindex $components 1] set $sym
                if {[llength $components] > 2 && [llength $args] > 0} {
                    $ns\::[lindex $components 2] set {*}$args
                }
            }
            lappend data [list $q0 $sym $q1 {*}$args]
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
        #: Set labeled or relative jumps to their final address.
        set _q [list]
        for {set i 0} {$i < [llength $data]} {incr i} {
            lassign [lindex $data $i] q0 - q1
            if {[regexp {^[-+]\d+$} $q1]} {
                lappend _q $q0
                lset data $i 2 [expr $q0$q1]
            } elseif {[dict exists $labels $q1]} {
                lappend _q $q0
                lset data $i 2 [dict get $labels $q1]
            } else {
                lappend _q $q0 $q1
            }
        }
        return [lsort -unique $_q]
    }

    method iterate args {
        #: Start a walk through the transition matrix.
        if {[lindex $args 0] eq "-steps"} {
            #: The option `-steps steps` is recognized: if given it limits the
            #: number of steps the walk will comprise.
            set args [lassign $args - steps]
        } else {
            set steps {}
        }
        #: Launch the recursive walk through the transition matrix. A list of
        #: IDs and a callback method from the machine class are provided as
        #: arguments. When the walk ends, results should contain a list of IDs
        #: that represent the endpoint(s) of the walk.
        set results [my Inner {*}$args]
        if {$steps ne {} && $steps > 0} {
            return -code error [format {premature stop with %d steps left} $steps]
        }
        return $results
    }

    method Inner {ids me} {
        log::log i [info level 0] 
        # ids  = moves into the point(s) where we are now
        # me = a callback method that queries the transition matrix and builds
        # new ids for the continuation of the transition walk.
        set newids [concat {*}[lmap id $ids {
            # Create ids for possible moves.
            my $me $id
        }]]
        # Two base cases: 1) no more ids, or 2) steps completed.
        if {[llength $newids] == 0 || ($steps ne {} && [incr steps -1] < 0)} {
            return [lsort -unique [lsort -index 1 $ids]]
        } else {
            # Recursive case.
            return [my Inner $newids $me]
        }
    }

}
