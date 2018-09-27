::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::PTM {
    mixin ::automata::Configuration ::automata::Machine

    #: A Post-Turing Machine is essentially a TM. The transition matrix is set
    #: by compiling a program.  The tape uses a binary symbol set (here, {0,
    #: 1}).
    #:
    #: The configuration of a PTM is (A, B, C, Q, E, S, F, H | t, h, q)

    constructor args {
        my graded "Tape symbols"  A -domain B
        my graded "Print symbols" B -enum {E P N}
        my graded "Move symbols"  C -enum {L R N}
        my graded "Instructions"  Q -domain N
        my graded "Program start" S -scalar
        my graded "Program end"   F
        my graded "Head position" H -domain N -default 0 -scalar
        my table -as {Q A Q B C}
        my id {
            t A* "tape"
            h H  "current cell"
            q Q  "current state"
        }
    }

    method compile tokens {
        #: Create a transition matrix from a sequence of operation tokens.
        #:
        set i 1
        set jumps [dict create]
        set program [list]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set jumps [string trimright $token :] $i
                continue
            }
            lassign [split $token :] op offset
            set next $i
            incr next
            # movement directions are switched
            switch $op {
                P - E {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next $op N]
                    }
                }
                L {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N R]
                    }
                }
                R {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N L]
                    }
                }
                N {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N N]
                    }
                }
                J {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $offset N N]
                    }
                }
                H {
                    my add F $next
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N N]
                    }
                }
                J0 {
                    foreach inp [my get A] {
                        if {$inp eq 0} {
                            lappend program [list $i $inp $offset N N]
                        } else {
                            lappend program [list $i $inp $next N N]
                        }
                    }
                }
                J1 {
                    foreach inp [my get A] {
                        if {$inp eq 1} {
                            lappend program [list $i $inp $offset N N]
                        } else {
                            lappend program [list $i $inp $next N N]
                        }
                    }
                }
                default {
                    error \$op=$op
                }
            }
            incr i
        }
        log::log d \$jumps=$jumps 
        my add S [lindex $jumps 1]
        my add F $i
        # fix jumps
        for {set i 0} {$i < [llength $program]} {incr i} {
            lassign [lindex $program $i] q0 - q1
            if {[regexp {^[-+]\d+$} $q1]} {
                lset program $i 2 [expr $q0$q1]
            } elseif {[dict exists $jumps $q1]} {
                lset program $i 2 [dict get $jumps $q1]
            }
        }
        # store program
        foreach line $program {
            my add table {*}$line
        }
    }

    method run {tape {tapeIndex {}}} {
        #: Run the code on this tape, return tape.
        if {$tapeIndex ne {}} {
            my add H $tapeIndex
        }
        set tape [list {*}$tape]
        set ids [lmap q [my get S] {
            my AddID $tape [my get H] $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id process
        }]]
        lmap result $results {
            dict with result {
                if {[my in F $q]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }

}
