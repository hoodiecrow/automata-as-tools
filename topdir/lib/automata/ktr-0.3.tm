::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::KTR {
    mixin ::automata::Configuration ::automata::Machine

    variable testlabels

    #: This is a very limited Karel the Robot that can only walk around, not
    #: interact with beepers.
    #:
    #: The configuration of a KTR is (A, B, C, Q, S, F | w, h, x, y, n, f, i, t, b, a)

    constructor args {
        my graded "Flag symbols"    A -domain B
        my graded "Lengths/Amounts" B -domain N -hide
        my graded "Facing"          C -enum {0 1 2 3}
        my graded "Instructions"    Q -domain N
        my graded "Program start"   S -scalar
        my graded "Program end"     F
        my graded "Operator list"   O -enum {
            TURN MOVE TAKE DROP TEST JUMPZ JUMP RET GOSUB NOP
        }
        my graded "Test numbers"    T -domain N
        my table -as {Q A Q O B}
        # world width, height, robot x, y, #beepers, facing, instruction stack, test state, beeper coords, wall coords
        my id {w h x y n f i t b a} \
              {B B B B B C Q* A B* B*}
        set testlabels {
            {}
            front-is-clear
            front-is-blocked
            left-is-clear
            left-is-blocked
            right-is-clear
            right-is-blocked
            next-to-a-beeper
            not-next-to-a-beeper
            facing-north
            not-facing-north
            facing-south
            not-facing-south
            facing-east
            not-facing-east
            facing-west
            not-facing-west
            any-beepers-in-beeper-bag
            no-beepers-in-beeper-bag
        }
    }

    method compile tokens {
        #: Convert source code to transition configuration.
        #:
        # address 0 is invalid
        set i 1
        set jumps [dict create]
        set program [list]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set jumps [string trimright $token :] $i
                continue
            }
            set next [expr {$i + 1}]
            switch $token {
                turnoff {
                    my add F $i
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next NOP 0]
                    }
                }
                turnleft {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next TURN 0]
                    }
                }
                move {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next MOVE 0]
                    }
                }
                pickbeeper {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next TAKE 0]
                    }
                }
                putbeeper {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next DROP 0]
                    }
                }
                RET {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next $token 0]
                    }
                }
                default {
                    if {[regexp {([[:upper:]]+):?(.*)$} $token -> op label]} {
                        if {$label eq {}} {
                            set label 0
                        }
                        switch $op {
                            TEST {
                                set n [my GetTestNumber $label]
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $next TEST $n]
                                }
                            }
                            JUMPZ {
                                lappend program [list $i 0 $next NOP 0]
                                lappend program [list $i 1 $label NOP 0]
                            }
                            JUMP {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $label NOP 0]
                                }
                            }
                            GOSUB {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $label $op 0]
                                }
                            }
                            NOP {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $next NOP 0]
                                }
                            }
                            default {
                                return -code error [format {unknown operation "%s"} $op]
                            }
                        }
                    } else {
                        return -code error [format {syntax error: "%s"} $token]
                    }
                }
            }
            incr i
        }
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

    method GetTestNumber label {
        lsearch $testlabels $label
    }

    method GetTestLabel index {
        lindex $testlabels $index
    }

    method run {world robot beepers walls {s {}}} {
        #: Run the code with the given configuration, starting from s.
        if {$s eq {}} {
            set s [my get S]
        }
        lappend id {*}$world
        lappend id {*}$robot
        lappend id [list $s]
        lappend id 0
        lappend id $beepers
        lappend id $walls
        set results [my search [my add id {*}$id] exec]
        set res [dict values [lindex $results 0]]
        list [lrange $res 0 1] [lrange $res 2 5] [lindex $res 6] [lindex $res 7] [lindex $res 8] [lindex $res 9]
    }

}