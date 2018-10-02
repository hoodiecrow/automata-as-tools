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
        set i $firstAddr
        set jumps [dict create]
        set program [dict create]
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
        my add S [lindex $jumps 1]
        my add F $i
        log::log d \$jumps=$jumps 
        log::log d \$program=$program 
        # fix jumps
        for {set n $firstAddr} {$n < $i} {incr n} {
            dict with program $n {
                lassign $label a b c
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
