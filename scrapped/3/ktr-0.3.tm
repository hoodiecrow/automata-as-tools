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
        my installOperations {JT: JNT: J: HALT TURN MOVE TAKE DROP TEST: RET CALL:}
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
        foreach token $tokens {
            my add operation firstAddr $token
        }
        my get operations
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
