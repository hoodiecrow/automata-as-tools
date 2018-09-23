::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::machine

namespace eval automata {}

oo::class create ::automata::FST {
    mixin ::automata::Printer
    variable epsilon

    #: A Finite State Transducer recognizes or encodes a regular relation.
    #:
    #: The ID of an FST is (a, q, b) = current input, current state, and current output.

    constructor args {
        set epsilon ε
        #: Recognized options:
        #:
        if {[lindex $args 0] eq "-epsilon"} {
            #: -epsilon c when the input symbol for an edge is this character,
            #: it is treated as an epsilon move. Default is ε.
            #:
            set args [lassign $args - epsilon]
        }
        #: This machine is defined by the tuple `<A, B, Q, S, F, T>`:
        #:
        ::automata::Component create A -label "Input symbols" -exclude {{}}
        ::automata::Component create B -label "Output symbols" -exclude {{}}
        ::automata::Component create Q -label "State symbols"
        ::automata::Component create S -label "Start symbols" -in [namespace which Q]
        ::automata::Component create F -label "Final symbols" -in [namespace which Q]
        ::automata::STE create T {Q S F A B}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the processing methods into T.
        oo::objdefine T mixin -append ::automata::Machine

    }

    method compile tokens {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and output
        #: input can contain one or more input symbols, separated by comma.
        #:
        foreach {from edge next} $tokens {
            regexp {([\w,]*)\s*/\s*([\w,]*)} $edge -> input output
            splitItems input
            splitItems output
            my T set $from $input $next {*}$output
        }
    }

    method run args {
        #: Run the machine:
        set _args [lassign $args arg]
        switch $arg {
            -recognize {
                #: provide the flag `-recognize` to recognize input/output stacks,
                my Recognize {*}$_args
            }
            -translate {
                #: provide the flag `-translate` to translate input and build output,
                my Translate {*}$_args
            }
            -reconstruct {
                #: provide the flag `-reconstruct` to reconstruct input from output,
                my Reconstruct {*}$_args
            }
            -generate {
                #: provide the flag `-generate` to generate input/output stacks for a given number of steps,
                my Generate {*}$_args
            }
            default {
                return -code error [format {provide a flag option to specify how to run the machine}]
            }
        }
        #: Also provide a list of input symbols.
    }

    method Recognize {a b} {
        # Are we in a final state when all input symbols in a and b are consumed?
        set a [list {*}$a]
        set b [list {*}$b]
        set ids [lmap s [my S get] {list $a $s $b}]
        foreach result [my T iterate $ids recognize] {
            lassign $result a q b
            if {[llength $a] == 0 && [my F contains $q] && [llength $b] == 0} {
                return 1
            }
        }
        return 0
    }

    method Translate a {
        # What symbols have been added to b when all input symbols in a are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s {}}]
        lmap result [my T iterate $ids translate] {
            lassign $result a q b
            if {[llength $a] == 0 && [my F contains $q]} {
                set b
            } else {
                continue
            }
        }
    }

    method Reconstruct b {
        # What symbols have been added to a when all input symbols in b are consumed?
        set b [list {*}$b]
        set ids [lmap s [my S get] {list {} $s $b}]
        lmap result [my T iterate $ids reconstruct] {
            lassign $result a q b
            if {$q in [my F get] && [llength $b] == 0} {
                set a
            } else {
                continue
            }
        }
    }

    method Generate steps {
        # If we take N steps into the transition sequence (or sequence powerset), what to we get in a and b?
        set ids [lmap s [my S get] {list {} $s {}}]
        set results [my T iterate -steps $steps $ids generate]
        return [lselect result {[lindex $result 1] in [my F get]} $results]
    }

#: * `A`, `B`, `Q`, `S`, `F`, `T` : public methods to give access to the components.

}
