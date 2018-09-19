::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::processor

namespace eval automata {}

oo::class create ::automata::CM {
    mixin ::automata::Printer

    #: A Counter Machine is the simplest form of Register Machine.
    #:
    #: The ID of a CM is (r, s) = current registers, current state.

    constructor args {
        #: This machine is defined by the tuple `<A, Q, S, T>`:
        ::automata::Component create A -label "Operations used" -exclude {}
        #: * *A* is the set of operations used.
        ::automata::Component create Q -label "State symbols"
        #: * *Q* is the set of state symbols (in this machine, this means instruction addresses).
        ::automata::Component create S -label "Program start" -in [namespace which Q] -scalar
        S set 0
        #: * *S* holds first instruction address.
        ::automata::STE create T {Q A}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the ExecCounter method into T.
        oo::objdefine T method ExecCounter id {
            lassign $id regs q0
            lassign [lindex [my get $q0 *] 0] - op addr reg
            switch $op {
                INC {
                    lset regs $reg [expr {[lindex $regs $reg] + 1}]
                    set q1 [expr {$q0 + 1}]
                }
                DEC {
                    lset regs $reg [expr {[lindex $regs $reg] - 1}]
                    set q1 [expr {$q0 + 1}]
                }
                JZ {
                    if {[lindex $regs $reg] eq 0} {
                        set q1 $addr
                    } else {
                        set q1 [expr {$q0 + 1}]
                    }
                }
                JE {
                    lassign [split $reg ,] r0 r1
                    if {[lindex $regs $r0] eq [lindex $regs $r1]} {
                        set q1 $addr
                    } else {
                        set q1 [expr {$q0 + 1}]
                    }
                }
                CLR {
                    lset regs $reg 0
                    set q1 [expr {$q0 + 1}]
                }
                CPY {
                    lassign [split $reg ,] r0 r1
                    lset regs $r1 [lindex $regs $r0]
                    set q1 [expr {$q0 + 1}]
                }
                HALT {
                    return
                }
                default {
                    error \$op=$op
                }
            }
            my addNewIDs [list $regs $q1]
        }

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
            regexp {([[:upper:]]+):?(\d*),?(\w*)$} $token -> op reg offset
            if {$offset eq {}} {
                set offset 0
            }
            T set $i $op $offset $reg
            incr i
        }
        my Q clear
        my Q set {*}[my T fixJumps $labels]
    }

    method run {regs {s {}}} {
        #: Run the code with the given register settings, starting from s.
        if {$s ne {}} {
            my S set $s
        }
        set ids [list [list $regs [my S get]]]
        set results [my T iterate $ids ExecCounter]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `T` : public methods to give access to the components.
}
