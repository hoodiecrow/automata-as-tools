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
        my installOperations {P E L R N H J: J0: J1:} {
            P   {list $i $j [incr i]                    P N} {print to current cell}
            E   {list $i $j [incr i]                    E N} {erase the current cell}
            L   {list $i $j [incr i]                    N R} {move head left}
            R   {list $i $j [incr i]                    N L} {move head right}
            N   {list $i $j [incr i]                    N N} {no operation}
            H   {list $i $j END_OF_CODE                 N N} {halt}
            J:  {list $i $j $a                          N N} {jump unconditionally to address *a*}
            J0: {list $i $j [if {!$j} {set a} {incr i}] N N} {jump on (<i>input</i> = 0) to address *a*}
            J1: {list $i $j [if {$j} {set a} {incr i}]  N N} {jump on (<i>input</i> = 1) to address *a*}
        }
        my installRunMethod {
            tape {} {a list of initial tape symbols}
            ?head? {} {initial head position}
        }
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
            if {![regexp {^([[:upper:]]\d?:)(.+)$} $token -> op offset]} {
                if {[regexp {^[[:upper:]]$} $token]} {
                    set op $token
                    set offset {}
                } else {
                    return -code error [format {malformed token "%s"} $token]
                }
            }
            lappend program {*}[lmap a [my get A] {my GenOp $i $a $op $offset}]
            incr i
        }
        dict set jumps END_OF_CODE $i
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
