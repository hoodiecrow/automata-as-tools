::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::ste
package require automata::component

namespace eval automata {}

oo::class create ::automata::FST {
    variable data

#: A Finite State Transducer recognizes or encodes a regular relation.

    constructor args {
#: This machine is defined by the tuple `<A, B, Q, S, F, T>`:
        ::automata::Component create A -label "Input alphabet" -nonempty
#: * *A* is the input alphabet (does not accept the empty string as symbol).
        ::automata::Component create B -label "Output alphabet" -nonempty
#: * *B* is the output alphabet (does not accept the empty string as symbol).
        ::automata::Component create Q -label "State symbols"
#: * *Q* is the set of state symbols.
        ::automata::Component create S -label "Start symbol(s)" -in [namespace which Q]
#: * *S* is a symbol which is a member of the set of state symbols (for a deterministic FST) or a set of symbols which is a subset of the state symbols (for a nondeterministic FST). Processing will start at this/these symbols.
        ::automata::Component create F -label "Final symbol(s)" -in [namespace which Q]
#: * *F* is a set of symbols which is a subset of *Q*. These are the accepting final states.
        ::automata::STE create T [self namespace] {Q A B}
#: * *T* is the transition relation, an instance of the `STE` class.
    }

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c {A B Q S F T} {my $c print}] \n]
    }

    method recognize {a b} {
        #: Are we in a final state when all input symbols in a and b are consumed?
        set results [my T iterate $a [my S get] $b [my F get] Consume Consume]
        foreach result $results {
            lassign $result a q b
            if {[llength $a] == 0 && [llength $b] == 0} {
                return 1
            }
        }
        return 0
    }

    method translate a {
        #: What symbols have been added to b when all input symbols in a are consumed?
        set results [my T iterate $a [my S get] {} [my F get] Consume Produce]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct b {
        #: What symbols have been added to a when all input symbols in b are consumed?
        set results [my T iterate {} [my S get] $b [my F get] Produce Consume]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        #: If we take N steps into the transition sequence (or sequence powerset), what to we get in a and b?
        my T iterate -steps $steps {} [my S get] {} [my F get] Produce Produce
    }

    foreach m {A B Q S F T} {
        forward $m $m ; export $m
    }
#: * `A`, `B`, `Q`, `S`, `F`, `T` : public methods to give access to the components.

}
