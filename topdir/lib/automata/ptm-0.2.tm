::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.1
package require automata::component

namespace eval automata {}

oo::class create ::automata::PTM {
    variable data instructions

    #: A Post-Turing Machine is essentially a TM. The transition matrix is set
    #: by compiling a program.  The tape uses a binary symbol set (here, {0,
    #: 1}).

    constructor args {
        #: This machine is defined by the tuple `<A, b, Q, S, F, T>`:
        ::automata::Component create A -label "Tape alphabet" -exclude {}
        A set 0 1
        #: * *A* is the tape alphabet (does not accept the empty string as symbol).
        ::automata::Component create b -label "Blank symbol" -scalar
        b set 0
        #: * *b* is the blank symbol in the tape alphabet.
        ::automata::Component create Q -label "State symbols"
        #: * *Q* is the set of state symbols (in this machine, this means instruction addresses).
        ::automata::Component create S -label "Program start" -in [namespace which Q] -scalar
        S set 1
        #: * *S* holds first instruction address.
        ::automata::Component create F -label "Program end" -in [namespace which Q] -scalar
        #: * *F* holds the address where the program halts.
        ::automata::STE create T [self namespace] {Q A}
        #: * *T* is the transition relation, an instance of the `STE` class.
    }

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c {A b Q S F T} {my $c print}] \n]
    }

    method compile tokens {
        #: Create a transition matrix from a sequence of operation tokens.
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
            switch $op {
                P {
                    T set $i [A get] $next [lindex [A get] end] N
                }
                E {
                    T set $i [A get] $next $op N
                }
                L - R {
                    T set $i [A get] $next N $op
                }
                N {
                    T set $i [A get] $next N N
                }
                J {
                    T set $i [A get] $offset N N
                }
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
        T fixJumps $labels
    }

    #: The ID of a PTM is (t, q, h) = current tape, current state, and current head.

    method run {tape {tapeIndex 0}} {
        #: Run the code on this tape, return tape.
        set result [lindex [my T iterate [linsert $tape 0 $tapeIndex] [S get] {} {} MatchTape PrintMove NoOp] 0 0]
        return [list [lrange $result 1 end] [lindex $result 0]]
    }

    foreach m {A b Q S F T} {
        forward $m $m ; export $m
    }

#: * `A`, `b`, `Q`, `S`, `F`, `T` : public methods to give access to the components.
}
