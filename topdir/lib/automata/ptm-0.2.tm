::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::operator

namespace eval automata {}

oo::class create ::automata::PTM {
    mixin ::automata::Printer
    variable instructions

    #: A Post-Turing Machine is essentially a TM. The transition matrix is set
    #: by compiling a program.  The tape uses a binary symbol set (here, {0,
    #: 1}).
    #:
    #: The ID of a PTM is (t, q, h) = current tape, current state, and current head.

    constructor args {
        #: This machine is defined by the tuple `<A, b, Q, S, F, T>`:
        #:
        ::automata::Component create A -label "Tape symbols" -domain B
        ::automata::Component create b -label "Blank symbol" -scalar -in A
        b set 0
        ::automata::Component create Q -label "Instructions" -domain N
        ::automata::Component create S -label "Program start" -in Q -scalar
        ::automata::Component create F -label "End points" -in Q
        ::automata::STE create T
        #: 
        #: Inject the Blank method and Operator class into T.
        oo::objdefine T method Blank {} [format {
            return [eval %s]
        } [list [namespace which b] get]]

        oo::objdefine T mixin -append ::automata::Operator

    }

    method compile tokens {
        #: Create a transition matrix from a sequence of operation tokens.
        #:
        set i 1
        set labels {}
        set instructions [list {}]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set labels [string trimright $token :] $i
                continue
            }
            lassign [split $token :] op offset
            set next $i
            incr next
            # movement directions are switched
            switch $op {
                P { T set $i [A get] $next [lindex [A get] end] N }
                E { T set $i [A get] $next [b get] N }
                L { T set $i [A get] $next N R }
                R { T set $i [A get] $next N L }
                N { T set $i [A get] $next N N }
                J { T set $i [A get] $offset N N }
                H {
                    F set $next
                    T set $i [A get] $next N N
                }
                J0 {
                    foreach inp [A get] {
                        if {$inp eq 0} {
                            T set $i $inp $offset N N
                        } else {
                            T set $i $inp $next N N
                        }
                    }
                }
                J1 {
                    foreach inp [A get] {
                        if {$inp eq 1} {
                            T set $i $inp $offset N N
                        } else {
                            T set $i $inp $next N N
                        }
                    }
                }
                default {
                    error \$op=$op
                }
            }
            incr i
        }
        my Q clear
        my Q set {*}[my T fixJumps $labels]
        my S set [lindex $labels 1]
        my F set $i
    }

    method run {tape {tapeIndex 0}} {
        #: Run the code on this tape, return tape.
        set tape [list {*}$tape]
        set ids [list [list $tape [my S get] $tapeIndex]]
        set results [my T iterate $ids process]
        set results [lselect result {[my F contains [lindex $result 1]]} $results]
    }

#: * `A`, `b`, `Q`, `S`, `F`, `T` : public methods to give access to the components.
}
