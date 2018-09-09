::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::ste
package require automata::component

namespace eval automata {}

oo::class create ::automata::PDA {
    variable data

    #: A Pushdown Automaton recognizes a context-free language.

    constructor args {
#: This machine is defined by the tuple `<A, B, Q, Z, S, F, T>`:

        ::automata::Component create A -nonempty
#: * *A* is the input alphabet (does not accept the empty string as symbol).
        ::automata::Component create B -nonempty
#: * *B* is the stack alphabet (does not accept the empty string as symbol).
        ::automata::Component create Q
#: * *Q* is the set of state symbols.
        ::automata::Component create Z -in [namespace which B] -scalar
#: * *Z* is a symbol which is a member of the set of stack symbols. The stack will contain this symbol when starting.
        ::automata::Component create S -in [namespace which Q]
#: * *S* is a symbol which is a member of the set of state symbols. Processing will start at this symbol.
        ::automata::Component create F -in [namespace which Q]
#: * *F* is a set of symbols which is a subset of *Q*. These are the accepting final states.
        ::automata::STE create T [self namespace] {Q A B}
#: * *T* is the transition relation, an instance of the `STE` class.
    }

    method accept a {
        #: Are we in a final state when all input symbols are consumed and the stack has only one item?
        set results [my T iterate $a [my S get] [my Z get] [my F get] Consume Pushdown]
        foreach result $results {
            lassign $result a q b
            if {[llength $a] == 0 && [llength $b] == 1} {
                return 1
            }
        }
        return 0
    }

    method classify a {
        #: What state are we in when all input symbols are consumed and the stack has only one item?
        set results [my T iterate $a [my S get] [my Z get] [my F get] Consume Pushdown]
        if {[llength $a] == 0} {
            return [lmap result $results {lindex $result 1}]
        } else {
            return {}
        }
    }

    foreach m {A B Q Z S F T} {
        forward $m $m ; export $m
    }

#: * `A`, `B`, `Q`, `Z`, `S`, `F`, `T` : public methods to give access to the components.
}
