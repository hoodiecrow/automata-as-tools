
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::Printer
    variable epsilon

    #: A Finite State Machine recognizes a regular language. It can be asked to
    #: accept or classify a list of input symbols.
    #:
    #: The ID of an FSM is (w, q) = remaining input and current state.
    #:
    #: Every element in ids represents a separate machine, all working in parallel.

    constructor args {
        set epsilon ε
        #: Recognized options:
        #:
        if {[lindex $args 0] eq "-epsilon"} {
            #: -epsilon c when the input symbol for an edge is this character, it is treated as an epsilon move. Default is ε.
            #:
            set args [lassign $args - epsilon]
        }
        #: This machine is defined by the tuple `<A, Q, S, F, T>`:
        #:
        ::automata::Component create A -label "Input symbols" -exclude {{}}
        ::automata::Component create Q -label "State symbols"
        ::automata::Component create S -label "Start symbols" -in [namespace which Q]
        ::automata::Component create F -label "Final symbols" -in [namespace which Q]
        ::automata::STE create T {Q A}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the makeMoves method into T.
        oo::objdefine T method makeMoves id {
            # unpack ID
            lassign $id a q0
            set _a [lassign $a A]
            # get epsilons
            set tuples [my get $q0 {}]
            # build new IDs
            my addNewIDs {*}[lmap tuple $tuples {
                list $a [lindex $tuple 2]
            }]
            # get moves
            set tuples [my get $q0 $A]
            # build new IDs
            my addNewIDs {*}[lmap tuple $tuples {
                list $_a [lindex $tuple 2]
            }]
        }

    }

    method compile tokens {
        #: 'source' form is three tokens: from, input, next.
        #: input can contain one or more input symbols, separated by comma.
        #:
        foreach {from input next} $tokens {
            splitItems input
            my T set $from $input $next
        }
    }

    method accept a {
        #: Are we in a final state when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s}]
        foreach result [my T iterate $ids makeMoves] {
            lassign $result a q
            if {[llength $a] == 0 && [my F contains $q]} {
                return 1
            }
        }
        return 0
    }

    method classify a {
        #: What state are we in when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s}]
        lmap result [my T iterate $ids makeMoves] {
            lassign $result a q
            if {[llength $a] == 0 && [my F contains $q]} {
                set q
            } else {
                continue
            }
        }
        return {}
    }

#: * `A`, `Q`, `S`, `F`, `T` : public methods to give access to the components.

}
