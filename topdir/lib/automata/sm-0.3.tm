::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::SM {
    mixin ::automata::Configuration ::automata::Machine

    #: A simple sort of virtual Stack Machine.
    #: 
    #: The configuration of an SM is (A, B, Q, S, F, O | s, i)
    #:
    #: The operations of the programming language are:
    #: 
    #: JZ:a, J:a  : jump on zero (top of stack) / jump unconditional to a
    #: INC, DEC   : increment/decrement top of stack
    #: CLR        : set top of stack to 0
    #: <integer>  : push value onto stack
    #: op         : (op = EQ, EQL, ADD, MUL, eq, ==, +, *) perform ALU op on top two stack elements and replace them with result

    constructor args {
        my graded "Flag symbols"  A -domain B
        my graded "Stack values"  B -domain N
        my graded "Instructions"  Q -domain N
        my graded "Program start" S -scalar
        my graded "Program end"   F
        my graded "Operator list" O -enum {PUSH INC DEC CLR NOP EQ EQL ADD MUL eq == + *}
        my table -as {Q A Q O B}
        my id {
            s B* "current stack"
            i Q  "instruction pointer"
        }
    }

    method compile tokens {
        #: Convert source code to transition configuration.
        set i 0
        set jumps [dict create]
        set program [list]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set jumps [string trimright $token :] $i
                continue
            }
            set next [expr {$i + 1}]
            if {$token in {eq == + *}} {
                foreach inp [my get A] {
                    lappend program [list $i $inp $next $token 0]
                }
            } else {
                if {[string is entier -strict $token]} {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next PUSH $token]
                    }
                } else {
                    if {[regexp {([[:upper:]]+):?(.*)$} $token -> op val]} {
                        switch $op {
                            JZ {
                                lappend program [list $i 0 $val NOP 0]
                                lappend program [list $i 1 $next NOP 0]
                            }
                            J {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $val NOP 0]
                                }
                            }
                            INC - DEC - CLR {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $next $op 0]
                                }
                            }
                            NOP {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $next NOP 0]
                                }
                            }
                            default {
                                error \$op=$op
                            }
                        }
                    } else {
                        error \$token=$token
                    }
                }
            }
            incr i
        }
        set next [expr {$i + 1}]
        foreach inp [my get A] {
            lappend program [list $i $inp $next NOP 0]
        }
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
        my add S [lindex [my get Q] 0]
    }

    method run {stack {s {}}} {
        #: Run the code with the given stack, starting from s.
        if {$s ne {}} {
            my set S $s
        }
        set id [my AddID $stack [my get S]]
        set results [my search $id ExecStack]
        dict values [lindex $results 0]
    }

}
