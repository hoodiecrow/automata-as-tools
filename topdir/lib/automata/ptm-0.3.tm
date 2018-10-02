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
        #: Create a transition matrix from a sequence of operation tokens.
        #:
        set firstAddr 1
        set i $firstAddr
        set jumps [dict create]
        set program [list]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set jumps [string trimright $token :] $i
                continue
            }
            if {![regexp {(\w+:?)(.*)} $token -> command label]} {
                return -code error [format {syntax error: %s} $token]
            }
            dict set program $i label [list {*}[string map {, { }} $label]]
            dict set program $i command $command
            incr i
        }
        dict set jumps END_OF_CODE $i
        log::log d \$jumps=$jumps 
        my add S [lindex $jumps 1]
        my add F $i
        # fix jumps
        for {set n $firstAddr} {$n < $i} {incr n} {
            dict with program $n {
                lassign $label a b
                set m [expr {$n + 1}]
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
                    PRINT {
                        set addresses [list $next $next]
                        set code [list PRINT: 1]
                    }
                    ERASE {
                        set addresses [list $next $next]
                        set code [list PRINT: 0]
                    }
                    ROLL: {
                        set addresses [list $next $next]
                        set code [list ROLL: $a]
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
