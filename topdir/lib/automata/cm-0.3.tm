::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::CM {
    mixin ::automata::Configuration ::automata::Machine

    variable instructionSet

    #: A Counter Machine is the simplest form of Register Machine.
    #:
    #: The configuration of a CM is (A, B, Q, E, S, F, R | r, i)
    #:
    #: The number of registers is determined by the length of the {0, 1} sequence passed to the `run` method. Registers are indexed in the same way as a list index (0, 1, 2, ...).
    #:
    #: Register #0 is reserved for the value 0. It is an error to set it to another value.

    constructor args {
        set is 4
        #: Specify which actual instruction set to use when instantiating machine.
        #:
        if {[lindex $args 0] eq "-instructionset"} {
            #: * `-instructionset 1` : {INC, DEC, JZ}, (Minsky (1961, 1967), Lambek (1961))
            #: * `-instructionset 2` : {CLR, INC, JE}, (Ershov (1958), Peter (1958) as interpreted by Shepherdson-Sturgis (1964); Minsky (1967); Schönhage (1980))
            #: * `-instructionset 3` : {INC, CPY, JE}, (Elgot-Robinson (1964), Minsky (1967))
            #: * `-instructionset 4` : {INC, DEC, CLR, CPY, J, JZ} (default: Shepherdson and Sturgis (1963))
            #:
            set args [lassign $args - is]
        }
        set instructionSet [dict get {
            1 {INC DEC JZ}
            2 {CLR INC JE}
            3 {INC CPY JE}
            4 {INC DEC CLR CPY J JZ}
        } $is]
        my graded "Register values" A -domain N
        my graded "Flag symbols"    B -domain B
        my graded "Instructions"    Q -domain N
        my graded "Erase symbol"    E -scalar -default 0
        my graded "Program start"   S -scalar
        my graded "Program end"     F
        my graded "Operator list"   O -enum {INC DEC CLR CPY NOP}
        my graded "Register index"  R -domain N
        my table -as {Q A Q O R*}
        # registers and instruction pointer
        my id {r i} {A* Q}

    }

    method compile tokens {
        #: Convert source code to transition configuration.
        #:
        set i 0
        set jumps [dict create]
        set program [list]
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set jumps [string trimright $token :] $i
                continue
            }
            regexp {([[:upper:]]+):?([,\d]*),?([-+]?\w*)$} $token -> op regs offset
            if {$offset eq {}} {
                set offset 0
            }
            set next [expr {$i + 1}]
            set regs [list {*}[string map {, { }} $regs]]
            #: The basic instruction set is INC, DEC, CLR, CPY, J, JZ, JE.
            #: The no operation instruction, NOP, is added to all sets to have something to jump to when jumping to end.
            if {$op ne "NOP" && $op ni $instructionSet} {
                return -code error [format {illegal instruction opcode "%s"} $op]
            }
            switch $op {
                INC - DEC - CLR - CPY {
                    foreach inp [my get B] {
                        lappend program [list $i $inp $next $op [linsert $regs 0 0]]
                    }
                }
                J {
                    foreach inp [my get B] {
                        lappend program [list $i $inp $offset NOP {0 0}]
                    }
                }
                JZ {
                    lappend program [list $i 0 $next NOP [linsert $regs 0 0]]
                    lappend program [list $i 1 $offset NOP [linsert $regs 0 0]]
                }
                JE { # JE not in Shepherdson and Sturgis
                    lappend program [list $i 0 $next NOP $regs]
                    lappend program [list $i 1 $offset NOP $regs]
                }
                NOP {
                    # not in Shepherdson and Sturgis
                    foreach inp [my get B] {
                        lappend program [list $i $inp $next NOP {0 0}]
                    }
                }
                default {
                    return -code error [format {invalid operation "%s"} $op]
                }
            }
            incr i
        }
        my add S [lindex $jumps 1]
        my add F $i
        log::log d \$program=$program 
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

    method run {regs {s {}}} {
        #: Run the code with the given register settings, starting from s.
        if {$s ne {}} {
            my add S $s
        }
        if no {
            set r [lindex [my T get [my S get] *] 0 4]
            set f [expr {[lindex $regs $r] != 0}]
            if no {
                set ids [list [list $regs [my S get] $f]]
            }
            set ids [list [list $regs [my get S]]]
        }
        set id [my AddID $regs [my get S]]
        set results [my search $id ExecCounter]
        dict values [lindex $results 0]
    }

}
