::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::processor

namespace eval automata {}

oo::class create ::automata::CM {
    mixin ::automata::Printer

    variable instructionSet

    #: A Counter Machine is the simplest form of Register Machine.
    #:
    #: The ID of a CM is (r, i) = current registers, current instruction pointer.
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
        #: This machine is defined by the tuple `<A, Q, S, F, T>`:
        ::automata::Component create A -label "Flag symbols" -domain B
        ::automata::Component create Q -label "Instructions" -domain N
        ::automata::Component create S -label "Program start" -in [namespace which Q] -scalar
        ::automata::Component create F -label "Program end" -in [namespace which Q] -scalar
        ::automata::STE create T {Q S F A}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the Processor class into T.
        oo::objdefine T mixin -append ::automata::Processor

    }

    method compile tokens {
        #: Convert source code to transition configuration.
        #:
        set i 0
        set labels {}
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set labels [string trimright $token :] $i
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
                    T set $i [A get] $next $op {*}$regs
                }
                J {
                    T set $i [A get] $offset
                }
                JZ {
                    T set $i 0 $next {} 0 [lindex $regs 0]
                    T set $i 1 $offset {} 0 [lindex $regs 0]
                }
                JE { # not in Shepherdson and Sturgis
                    T set $i 0 $next {} {*}$regs
                    T set $i 1 $offset {} {*}$regs
                }
                NOP { # not in Shepherdson and Sturgis
                    T set $i [A get] $next
                }
                default {
                    return -code error [format {invalid operation "%s"} $op]
                }
            }
            incr i
        }
        my Q clear
        my Q set {*}[my T fixJumps $labels]
        my S set [lindex $labels 1]
        my F set $i
    }

    method run {regs {s {}}} {
        #: Run the code with the given register settings, starting from s.
        if {$s ne {}} {
            my S set $s
        }
        set r [lindex [my T get [my S get] *] 0 4]
        set f [expr {[lindex $regs $r] != 0}]
        if no {
            set ids [list [list $regs [my S get] $f]]
        }
        set ids [list [list $regs [my S get]]]
        set results [my T iterate $ids ExecCounter]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `F`, `T` : public methods to give access to the components.
}
