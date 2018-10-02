::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::SM {
    mixin ::automata::Configuration ::automata::Machine

    constructor args {
        my add doc preamble {
A simple sort of virtual Stack Machine.
        }
        my installOperations {INC DEC CLR J: JZ: EQ EQL ADD MUL eq == + * PUSH NOP}
        if no {
            INC      {list $i $j [incr i] INC  0}  {<i>ToS</i> ← [<i>ToS</i>] + 1}
            DEC      {list $i $j [incr i] DEC  0}  {<i>ToS</i> ← [<i>ToS</i>] - 1}
            CLR      {list $i $j [incr i] PUSH 0}  {<i>ToS</i> ← 0}
            EQ       {list $i $j [incr i] EQ   0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] eq [<i>ToS<sub>1</sub></i>]}
            EQL      {list $i $j [incr i] EQL  0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] == [<i>ToS<sub>1</sub></i>]}
            ADD      {list $i $j [incr i] ADD  0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] + [<i>ToS<sub>1</sub></i>]}
            MUL      {list $i $j [incr i] MUL  0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] * [<i>ToS<sub>1</sub></i>]}
            eq       {list $i $j [incr i] EQ   0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] eq [<i>ToS<sub>1</sub></i>]}
            ==       {list $i $j [incr i] EQL  0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] == [<i>ToS<sub>1</sub></i>]}
            +        {list $i $j [incr i] ADD  0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] + [<i>ToS<sub>1</sub></i>]}
            *        {list $i $j [incr i] MUL  0}  {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] * [<i>ToS<sub>1</sub></i>]}
            NOP      {list $i $j [incr i] NOP  0}  {no operation}
            J:       {list $i $j $a       NOP  0}  {jump unconditionally to address *a*}
            JZ:      {list $i $j [if {!$j} {set a} {incr i}] NOP  0} {jump on (<i>ToS</i> = 0) to address *a*}
            <number> {list $i $j [incr i] PUSH $a} {<i>ToS</i> ← <i>number</i>}
        }
        my installRunMethod {
            stack {} {a list of initial stack symbols}
            ?start? {} {initial state}
        }
        my graded "Flag symbols"  A -domain B
        my graded "Stack values"  B -domain N -hide
        my graded "Instructions"  Q -domain N
        my graded "Program start" S -scalar
        my graded "End points"    F
        my graded "Operators"     O -enum {PUSH INC DEC CLR NOP EQ EQL ADD MUL eq == + *}
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
            set val 0
            switch $token {
                INC - DEC -
                NOP { set op $token }
                EQ - eq   { set op EQ }
                EQL - ==  { set op EQL }
                ADD - +   { set op ADD }
                MUL - *   { set op MUL }
                default {
                    if {[string is entier -strict $token]} {
                        set op PUSH
                        set val [expr {$token}]
                    } elseif {![regexp {([[:upper:]]+:)(.*)$} $token -> op val]} {
                        error \$token=$token
                    }
                }
            }
            lappend program {*}[lmap a [my get A] {my GenOp $i $a $op $val}]
            incr i
        }
        lappend program {*}[lmap a [my get A] {my GenOp $i $a NOP 0}]
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

    method Run {stack {s {}}} {
        #: Run the code with the given stack, starting from s.
        if {$s ne {}} {
            my set S $s
        }
        set id [my AddID $stack [my get S]]
        set results [my search $id ExecStack]
        dict values [lindex $results 0]
    }

}
