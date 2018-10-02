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
        my installOperations {INC DEC CLR J: JZ: EQ EQL ADD MUL PUSH NOP}
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
        my graded "Operators"     O -hide
        my table -as {Q A Q O*}
        my id {
            s B* "current stack"
            i Q  "instruction pointer"
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

    method Run {stack {s {}}} {
        #: Run the code with the given stack, starting from s.
        if {$s ne {}} {
            my set S $s
        }
        set id [my AddID $stack [my get S]]
        set results [my search $id SM-exec]
        dict values [lindex $results 0]
    }

}
