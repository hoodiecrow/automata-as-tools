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
        my graded "Turn symbols"    L -enum {L N} -hide
        my graded "Move flag"       M -domain B -hide
        my graded "Instructions"    Q -domain N
        my graded "Program start"   S -scalar
        my graded "Program end"     F
        my graded "Operator list"   O -enum {
            TAKE DROP TEST RET GOSUB NOP
        }
        my graded "Test numbers"    T -domain N -hide
        my table -as {Q A Q L M O B}
        my id {
            w B  "world width"
            h B  "world height"
            x B  "robot x"
            y B  "robot y"
            n B  "robot's beepers"
            f C  "robot facing"
            i Q* "instructions"
            t A  "test state"
            b B* "beeper coords"
            a B* "wall coords"
        }
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
                        lappend program [list $i $inp $next N 0 NOP 0]
                    }
                }
                turnleft {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next L 0 NOP 0]
                    }
                }
                move {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N 1 NOP 0]
                    }
                }
                pickbeeper {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N 0 TAKE 0]
                    }
                }
                putbeeper {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N 0 DROP 0]
                    }
                }
                RET {
                    foreach inp [my get A] {
                        lappend program [list $i $inp $next N 0 $token 0]
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
                                    lappend program [list $i $inp $next N 0 TEST $n]
                                }
                            }
                            JUMPZ {
                                lappend program [list $i 0 $next N 0 NOP 0]
                                lappend program [list $i 1 $label N 0 NOP 0]
                            }
                            JUMP {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $label N 0 NOP 0]
                                }
                            }
                            GOSUB {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $label N 0 $op 0]
                                }
                            }
                            NOP {
                                foreach inp [my get A] {
                                    lappend program [list $i $inp $next N 0 NOP 0]
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
