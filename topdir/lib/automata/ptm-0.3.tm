::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::PTM {
    mixin ::automata::Configuration ::automata::Machine

    constructor args {
        my add doc preamble {
A Post-Turing Machine is essentially a TM. The transition matrix
is set by compiling a program.  The tape uses a binary symbol set
(here, 0, 1).
        }
        my installOperations {PRINT ERASE ROLL NOP HALT J: JNT: JT:}
        my installRunMethod {
            tape {} {a list of initial tape symbols}
            ?head? {} {initial head position}
        }
        my graded "Tape symbols"  A -domain B
        my graded "Instructions"  Q -domain N
        my graded "Program start" S -scalar
        my graded "Program end"   F
        my graded "Head position" H -domain N -default 0 -scalar -hide
        my graded "Operations"    O -hide
        my table -as {Q A Q O*}
        my id {
            t A* "tape"
            h H  "current cell"
            q Q  "current state"
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

    method Run {tape {head {}}} {
        #: Run the code on this tape, return tape.
        if {$head ne {}} {
            my add H $head
        }
        set tape [list {*}$tape]
        set ids [lmap q [my get S] {
            my AddID $tape [my get H] $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id PTM-exec
        }]]
        lmap result $results {
            dict values $result
        }
    }

}
