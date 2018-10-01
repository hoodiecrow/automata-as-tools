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
        my installOperations {HALT TURN MOVE DROP RET TEST: JT: J: CALL:} {
            HALT  halt            {Stop the program}
            TURN  turn            {Changes robot's facing counter-clockwards}
            MOVE  move            {Moves the robot one space forward}
            TAKE  take            {Transfer a beeper from square to bag (does nothing)}
            DROP  drop            {Transfer a beeper from bag to square (does nothing)}
            RET   ret             {Return to previous address, sets flag}
            TEST: {test $d $e}    {Test}
            JT:   {jt   $a}       {Jump to address *a* on <i>test</i> = 0}
            J:    {je   $a 0 0}   {Jump to address *a*}
            CALL: {call $a $m}    {Call to address *a*, sets flag}
            NOP   nop             {No operation}
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
        set i 1
        set jumps [dict create]
        set program [list]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set jumps [string trimright $token :] $i
                continue
            }
            if {[regexp {(\w+:?)(.*)} $token -> command label]} {
                log::log d \$token=$token 
                set a 0
                set b 0
                set c 0
                switch $command {
                    turnoff    { set op HALT }
                    turnleft   { set op TURN }
                    move       { set op MOVE }
                    pickbeeper { set op TAKE }
                    putbeeper  { set op DROP }
                    RET        { set op RET }
                    TEST:      {
                        set op TEST:
                        set e 0
                        switch -regexp -matchvar m $label {
                            {^facing-(\w+)}     { set d 0 }
                            {^not-facing-(\w+)} { set d 1 }
                            {^front-is-(\w+)}   { set d 2 }
                            {^left-is-(\w+)}    { set d 3 }
                            {^right-is-(\w+)}   { set d 4 }
                            {^next-}            { set d 5 }
                            {^not-next-}        { set d 6 }
                            {^any-}             { set d 7 }
                            {^no-}              { set d 8 }
                            default {
                                return -code error [format {syntax error: %s} $token]
                            }
                        }
                        if {[llength $m] > 1} {
                            switch [lindex $m 1] {
                                east    { set e 0 }
                                north   { set e 1 }
                                west    { set e 2 }
                                south   { set e 3 }
                                clear   { set e 0 }
                                blocked { set e 1 }
                                default { set e 0 }
                            }
                        }
                    }
                    JUMPZ:     {
                        set op JT:
                        set a $label
                    }
                    JUMP:      {
                        set op J:
                        set a $label
                    }
                    CALL:      {
                        set op CALL:
                        set a $label
                    }
                    NOP        { set op NOP }
                    default {
                        return -code error [format {syntax error: %s} $token]
                    }
                }
            } else {
                return -code error [format {syntax error: %s} $token]
            }
            dict set program $i idxs [list $a $b $c $d $e]
            dict set program $i op $op
            incr i
        }
        dict set jumps END_OF_CODE $i
        my add S [lindex $jumps 1]
        my add F $i
        log::log d \$jumps=$jumps 
        # fix jumps
        for {set n 1} {$n < $i} {incr n} {
            dict with program $n {
                lassign $idxs a b c d e
                set m [expr {$n + 1}]
                log::log d \$idxs=$idxs 
                if {[regexp {^[-+]\d+$} $a]} {
                    set a [expr $n$a]
                } elseif {[dict exists $jumps $a]} {
                    set a [dict get $jumps $a]
                }
                set _op [my GenOp $op]
                log::log d \$_op=$_op 
                switch -glob $_op {
                    test* {
                        set o [list test $d $e]
                        set a $m
                    }
                    call* {
                        set o [subst $_op]
                        set m $a
                    }
                    turn - halt - ret - move {
                        set o [subst $_op]
                        set a $m
                    }
                    default {
                        set o [subst $_op]
                    }
                }
                foreach inp [my get A] {
                    my add table $n $inp [if {$inp && [llength $idxs] > 0} {set a} {set m}] $o
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
        set results [my search [my add id {*}$id] exec]
        set res [dict values [lindex $results 0]]
        list [lrange $res 0 1] [lrange $res 2 5] [lindex $res 6] [lindex $res 7] [lindex $res 8] [lindex $res 9]
    }

}
