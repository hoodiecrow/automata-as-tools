::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.1
package require automata::component

namespace eval automata {}

oo::class create ::automata::PDA {
    variable data

    #: A Pushdown Automaton recognizes a context-free language.

    constructor args {
#: This machine is defined by the tuple `<A, B, Q, Z, S, F, T>`:
        ::automata::Component create A -label "Input alphabet" -exclude {}
#: * *A* is the input alphabet (does not accept the empty string as symbol).
        ::automata::Component create B -label "Stack alphabet" -exclude {}
#: * *B* is the stack alphabet (does not accept the empty string as symbol).
        ::automata::Component create Q -label "State symbols"
#: * *Q* is the set of state symbols.
        ::automata::Component create Z -label "Stack bottom" -in [namespace which B] -scalar
#: * *Z* is a symbol which is a member of the set of stack symbols. The stack will contain this symbol when starting.
        ::automata::Component create S -label "Start symbol(s)" -in [namespace which Q]
#: * *S* is a symbol which is a member of the set of state symbols. Processing will start at this symbol.
        ::automata::Component create F -label "Final symbol(s)" -in [namespace which Q]
#: * *F* is a set of symbols which is a subset of *Q*. These are the accepting final states.
        ::automata::STE create T [self namespace] {Q A B}
#: * *T* is the transition relation, an instance of the `STE` class.
    }

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c {A B Q Z S F T} {my $c print}] \n]
    }

    #: The ID of a PDA is (w, q, s) = remaining input, current state, and current stack.

    method accept a {
        #: Are we in a final state when all input symbols are consumed and the stack has only one item?
        set results [my T iterate $a [my S get] [my Z get] [my F get] MatchTop Consume Pushdown]
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
        set results [my T iterate $a [my S get] [my Z get] [my F get] MatchTop Consume Pushdown]
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
