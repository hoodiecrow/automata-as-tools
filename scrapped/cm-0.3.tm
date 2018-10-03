::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::CM {
    mixin ::automata::Configuration ::automata::Machine

    variable instructionSet

    constructor args {
        set is 4
        if {[lindex $args 0] eq "-instructionset"} {
            set args [lassign $args - is]
        }
        set instructionSet [dict get {
            1 {INC: DEC: JZ:}
            2 {CLR: INC: JE:}
            3 {INC: CPY: JE:}
            4 {INC: DEC: CLR: CPY: J: JZ:}
        } $is]
        my add doc preamble {
A Counter Machine is the simplest form of Register Machine.

The number of registers is determined by the length of the list passed to the `run` method. Registers are indexed in the same way as a list index (0, 1, 2, ...).

Register #0 is reserved for the value 0. It is an error to set it to another value.

Specify which actual instruction set to use when instantiating machine.

* `-instructionset 1` : (INC, DEC, JZ), (Minsky (1961, 1967), Lambek (1961))
* `-instructionset 2` : (CLR, INC, JE), (Ershov (1958), Peter (1958) as interpreted by Shepherdson-Sturgis (1964); Minsky (1967); Schönhage (1980))
* `-instructionset 3` : (INC, CPY, JE), (Elgot-Robinson (1964), Minsky (1967))
* `-instructionset 4` : (INC, DEC, CLR, CPY, J, JZ) (default: Shepherdson and Sturgis (1963))
        }
        my installOperations $instructionSet
        my installRunMethod {
            registers {} {a list of initial register cells}
            ?start? {} {initial state}
        }
        my graded "Flag symbols"    A -domain B
        my graded "Register values" B -domain N -hide
        my graded "Instructions"    Q -domain N
        my graded "Erase symbol"    E -scalar -default 0
        my graded "Program start"   S -scalar
        my graded "Program end"     F
        my graded "Operations"      O -hide
        my table -as {Q A Q O*}
        my id {
            r B* "registers"
            i Q  "instruction pointer"
        }

    }

    method compile tokens {
        #: Convert source code to transition configuration.
        #:
        set firstAddr 0
        foreach token $tokens {
            my add operation firstAddr $token
        }
        my get operations
    }

    method Run {regs {s {}}} {
        #: Run the code with the given register settings, starting from s.
        if {$s ne {}} {
            my add S $s
        }
        set id [my add id $regs [my get S]]
        set results [my search $id CM-exec]
        dict values [lindex $results 0]
    }

}
