
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component

namespace eval automata {}

oo::class create ::automata::FSM {
    variable data

#: A Finite State Machine recognizes a regular language. It can be asked to accept or classify a list of input symbols.

    constructor args {
#: This machine is defined by the tuple `<A, Q, S, F, T>`:
        ::automata::Component create A -label "Input alphabet" -exclude {}
#: * *A* is the input alphabet (does not accept the empty string as symbol).
        ::automata::Component create Q -label "State symbols"
#: * *Q* is the set of state symbols.
        ::automata::Component create S -label "Start symbol(s)" -in [namespace which Q]
#: * *S* is a symbol which is a member of the set of state symbols (for a deterministic FSM) or a set of symbols which is a subset of the state symbols (for a nondeterministic FSM). Processing will start at this/these symbols.
        ::automata::Component create F -label "Final symbol(s)" -in [namespace which Q]
#: * *F* is a set of symbols which is a subset of *Q*. These are the accepting final states.
        ::automata::STE create T [self namespace] {Q A}
#: * *T* is the transition relation, an instance of the `STE` class.
    }

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c {A Q S F T} {my $c print}] \n]
    }

    method compile tokens {
        foreach {q0 sym q1} $tokens {
            my T set $q0 $sym $q1
        }
    }

    #: The ID of an FSM is (w, q) = remaining input and current state.
    #: Every element in ids represents a separate machine, all working in parallel.

    method Accept id {
        lassign $id a q0
        set tuples [my T get $q0 {}]
        set q1s [lmap tuple $tuples {lindex $tuple 2}]
        my T addNewIDs {*}[lmap q1 $q1s {list $a $q1}]
        set tuples [my T get $q0 [lindex $a 0]]
        set q1s [lmap tuple $tuples {lindex $tuple 2}]
        if {[llength $q1s] > 0} {
            set a [lrange $a 1 end]
        }
        my T addNewIDs {*}[lmap q1 $q1s {list $a $q1}]
    }

    method accept a {
        #: Are we in a final state when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s}]
        set results [my T iterate $ids [namespace code [list my Accept]]]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        foreach result $results {
            lassign $result a
            if {[llength $a] == 0} {
                return 1
            }
        }
        return 0
    }

    method classify a {
        #: What state are we in when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s}]
        set results [my T iterate $ids [namespace code [list my Accept]]]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        if {[llength $a] == 0} {
            return [lmap result $results {lindex $result 1}]
        } else {
            return {}
        }
    }

    foreach m {A Q S F T} {
        forward $m $m ; export $m
    }

#: * `A`, `Q`, `S`, `F`, `T` : public methods to give access to the components.

}
