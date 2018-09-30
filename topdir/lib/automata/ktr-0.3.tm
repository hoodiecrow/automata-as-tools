::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::KTR {
    mixin ::automata::Configuration ::automata::Machine

    variable testlabels

    constructor args {
        my add doc preamble {
This is a very limited Karel the Robot that can only walk around, not interact with beepers.

Test numbers:

| Number | Test                      |
| :---:  | :---                      |
| 1/2    | front-is-clear/blocked    |
| 3/4    | left-is-clear/blocked     |
| 5/6    | right-is-clear/blocked    |
| 7/8    | next-to-a-beeper/not-     |
| 9/10   | facing-north/not-         |
| 11/12  | facing-south/not-         |
| 13/14  | facing-east/not-          |
| 15/16  | facing-west/not-          |
| 17     | any-beepers-in-beeper-bag |
| 18     | no-beepers-in-beeper-bag  |
        }
        my installOperations {turnoff turnleft move pickbeeper putbeeper RET TEST: JUMPZ: JUMP: GOSUB:} {
            turnoff    {list $i $j END_OF_CODE                N 0 NOP   0 } {shut down the robot (and end the program)}
            turnleft   {list $i $j [incr i]                   L 0 NOP   0 } {turn the robot left}
            move       {list $i $j [incr i]                   N 1 NOP   0 } {move the robot forward}
            pickbeeper {list $i $j [incr i]                   N 0 TAKE  0 } {pick up a beeper (does nothing)}
            putbeeper  {list $i $j [incr i]                   N 0 DROP  0 } {place a beeper (does nothing)}
            RET        {list $i $j [incr i]                   N 0 RET   0 } {return from a subroutine}
            TEST:      {list $i $j [incr i]                   N 0 TEST  $a} {perform test *a*}
            JUMPZ:     {list $i $j [if {$j} {set a} {incr i}] N 0 NOP   0 } {jump on (test flag =) zero to address *a*}
            JUMP:      {list $i $j $a                         N 0 NOP   0 } {jump unconditionally to address *a*}
            GOSUB:     {list $i $j $a                         N 0 GOSUB 0 } {jump to subroutine at address *a*}
            NOP        {list $i $j [incr i]                   N 0 NOP   0 } {no operation}
        }
        my installRunMethod {
            world   {} {a list of width, height values (integer)}
            robot   {} {a list of x, y, n, f values (integer)}
            beepers {} {an even-sized list of x, y values}
            walls   {} {an even-sized list of x, y values}
            ?start? {} {initial state}
        }
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
            if {$token eq "turnoff"} {
                set op $token
                set val 0
                my add F $i
            } elseif {$token in {turnleft move pickbeeper putbeeper RET NOP}} {
                set op $token
                set val 0
            } elseif {[regexp {(TEST:)(.*)$} $token -> op label]} {
                set val [my GetTestNumber $label]
            } elseif {![regexp {([[:upper:]]+:)(.*)$} $token -> op val]} {
                error \$token=$token
            }
            lappend program {*}[lmap a [my get A] {my GenOp $i $a $op $val}]
            incr i
        }
        dict set jumps END_OF_CODE $i
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
        set results [my search [my add id {*}$id] exec]
        set res [dict values [lindex $results 0]]
        list [lrange $res 0 1] [lrange $res 2 5] [lindex $res 6] [lindex $res 7] [lindex $res 8] [lindex $res 9]
    }

}
