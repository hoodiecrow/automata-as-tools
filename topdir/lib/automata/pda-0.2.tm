::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::machine

namespace eval automata {}

oo::class create ::automata::PDA {
    mixin ::automata::Printer
    variable epsilon

    #: A Pushdown Automaton recognizes a context-free language.
    #:
    #: The ID of a PDA is (w, q, s) = remaining input, current state, and current stack.

    constructor args {
        set epsilon ε
        #: Recognized options:
        #:
        if {[lindex $args 0] eq "-epsilon"} {
            #: -epsilon c when the input symbol for an edge is this character, it is treated as an epsilon move. Default is ε.
            #:
            set args [lassign $args - epsilon]
        }
        #: This machine is defined by the tuple `<A, B, Q, Z, S, F, T>`:
        #:
        ::automata::Component create A -label "Input symbols" -exclude {{}}
        ::automata::Component create B -label "Stack symbols" -exclude {{}}
        ::automata::Component create Q -label "State symbols"
        ::automata::Component create Z -label "Stack bottom" -in [namespace which B] -scalar
        ::automata::Component create S -label "Start symbol" -in [namespace which Q] -scalar
        ::automata::Component create F -label "Final symbols" -in [namespace which Q]
        ::automata::STE create T {Q A B}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the Machine class into T.
        oo::objdefine T mixin -append ::automata::Machine

    }

    method compile tokens {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and stack-action
        #: input can contain one or more input symbols, separated by comma.
        #: stack-action is split by ; into stack-input and stack-push
        #: Stack symbols in stack-push are separated by commas.
        foreach {from edge next} $tokens {
            regexp {([\w,]*)\s*/\s*(\w*);([\w,]*)} $edge -> input stackInput stackPush
            splitItems input
            splitItems stackPush
            my T set $from $input $next $stackInput {*}$stackPush
        }
    }

    method accept a {
        #: Are we in a final state when all input symbols are consumed and the stack has only one item?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s [list [my Z get]]}]
        foreach result [my T iterate $ids makeMoves] {
            lassign $result a q b
            if {[llength $a] == 0 && [my F contains $q] && [llength $b] == 1} {
                return 1
            }
        }
        return 0
    }

    method classify a {
        #: What state are we in when all input symbols are consumed and the stack has only one item?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s [list [my Z get]]}]
        lmap result [my T iterate $ids makeMoves] {
            lassign $result a q b
            if {[llength $a] == 0 && [my F contains $q] && [llength $b] == 1} {
                set q
            } else {
                continue
            }
        }
        return {}
    }

#: * `A`, `B`, `Q`, `Z`, `S`, `F`, `T` : public methods to give access to the components.
}
