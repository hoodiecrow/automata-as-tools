
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::machine

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::Printer
    variable epsilon

    #: A [[Finite State Machine|finitestatemachine]] recognizes a regular
    #: language. It can be asked to accept (respond with 1 if the machine
    #: recognizes the input, otherwise 0) or classify (respond with the final
    #: state) a list of input symbols.
    #:
    #: The ID of an FSM is (w, q) = remaining input and current state.

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
        ::automata::STE create T {Q S F A}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the Machine class into T.
        oo::objdefine T mixin -append ::automata::Machine

    }

    method compile tokens {
        #: 'source' form is three tokens: from, input, next.
        #: input can contain one or more input symbols, separated by comma.
        #: Put a `<` character before the state symbol to signify a start
        #: state, and/or a `>` character after it to signify a final state.
        #:
        foreach {from input next} $tokens {
            splitItems input
            my T set $from $input $next
        }
    }

    method run args {
        #: Run the machine:
        set _args [lassign $args arg]
        switch $arg {
            -acceptor - -accept {
                #: provide the flag `-acceptor` or `-accept` to accept input,
                my Accept {*}$_args
            }
            -classifier - -classify {
                #: and `-classifier` or `-classify` to classify input.
                my Classify {*}$_args
            }
            default {
                #: With no flags given, the machine accepts.
                my Accept {*}$args
            }
        }
        #: Also provide a list of input symbols.
    }

    method Accept a {
        # Are we in a final state when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s}]
        foreach result [my T iterate $ids consumeOne] {
            lassign $result a q
            if {[llength $a] == 0 && [my F contains $q]} {
                return 1
            }
        }
        return 0
    }

    method Classify a {
        # What state are we in when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s}]
        lmap result [my T iterate $ids consumeOne] {
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
