::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::KTR {
    mixin ::automata::Configuration ::automata::Machine

    constructor args {
        my add doc preamble {
This is a very limited Karel the Robot that can only walk around, not interact with beepers.

Test numbers:

| Number | Test                      |
| :---:  | :---                      |
| 0    | front-is-clear    |
| 1    | left-is-clear     |
| 2    | right-is-clear    |
| 3    | next-to-a-beeper     |
| 4  | facing-east          |
| 5   | facing-north         |
| 6  | facing-west          |
| 7  | facing-south         |
| 8     | any-beepers-in-beeper |
        }
        my installOperations {HALT TURN MOVE DROP RET TEST: JT: J: CALL:}
        my installRunMethod {
            world   {} {a list of width, height values (integer)}
            robot   {} {a list of x, y, n, f values (integer)}
            beepers {} {an even-sized list of x, y values}
            walls   {} {an even-sized list of x, y values}
            ?start? {} {initial state}
        }
        my graded "Flag symbols"    A -domain B
        my graded "Lengths/Amounts" B -domain N -hide
        my graded "Facing"          C -enum {0 1 2 3} -hide
        my graded "Instructions"    Q -domain N
        my graded "Program start"   S -scalar
        my graded "Program end"     F
        my graded "Operations"      O -hide
        my table -as {Q A Q O*}
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
    }

    method compile tokens {
        #: Convert source code to transition configuration.
        #:
        # address 0 is invalid
        set firstAddr 1
        set i $firstAddr
        set jumps [dict create]
        set program [list]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set jumps [string trimright $token :] $i
                continue
            }
            log::log d \$token=$token 
            if {![regexp {(\w+:?)(.*)} $token -> command label]} {
                return -code error [format {syntax error: %s} $token]
            }
            dict set program $i label [list {*}[string map {, { }} $label]]
            dict set program $i command $command
            incr i
        }
        dict set jumps END_OF_CODE $i
        my add S [lindex $jumps 1]
        my add F $i
        log::log d \$jumps=$jumps 
        # fix jumps
        for {set n $firstAddr} {$n < $i} {incr n} {
            dict with program $n {
                lassign $label a b c d e
                log::log d \$label=$label 
                if {[regexp {^[-+]\d+$} $a]} {
                    set a [expr $n$a]
                } elseif {[dict exists $jumps $a]} {
                    set a [dict get $jumps $a]
                }
                set next [expr {$n + 1}]
                switch $command {
                    JE: - JZ: {
                        # JE has two valid args, JZ one
                        set addresses [list $next $a]
                        set code [list $command $b $c]
                    }
                    JT: {
                        set addresses [list $next $a]
                        set code NOP
                    }
                    JNE: - JNZ: {
                        set addresses [list $a $next]
                        set code [list J[string index $command end] $b $c]
                    }
                    JNT: {
                        set addresses [list $a $next]
                        set code NOP
                    }
                    J: {
                        set addresses [list $a $a]
                        set code NOP
                    }
                    CALL: {
                        set addresses [list $a $a]
                        set code $command
                    }
                    TEST: {
                        set addresses [list $next $next]
                        set code [list $command [lsearch -exact {
                            front-is-clear
                            left-is-clear
                            right-is-clear
                            next-to-a-beeper
                            facing-north
                            facing-south
                            facing-east
                            facing-west
                            any-beepers-in-beeper-bag
                        } $label]]
                    }
                    INC: - DEC: - CLR: {
                        set addresses [list $next $next]
                        set code [list $command $a]
                    }
                    CPY: {
                        set addresses [list $next $next]
                        set code [list $command $a $b]
                    }
                    default {
                        set addresses [list $next $next]
                        set code [list $command]
                    }
                }
                if {![info exists code]} {
                    error [list $command $label]
                }
                set code [lmap c $code {
                    if {$c eq {}} {
                        continue
                    } else {
                        set c
                    }
                }]
                foreach inp [my get A] addr $addresses {
                    my add table $n $inp $addr $code
                }
            }
        }
    }

    method Run {world robot beepers walls {s {}}} {
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
        set results [my search [my add id {*}$id] KTR-exec]
        set res [dict values [lindex $results 0]]
        list [lrange $res 0 1] [lrange $res 2 5] [lindex $res 6] [lindex $res 7] [lindex $res 8] [lindex $res 9]
    }

}
