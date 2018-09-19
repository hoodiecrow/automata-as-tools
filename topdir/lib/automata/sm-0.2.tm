::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::processor

namespace eval automata {}

oo::class create ::automata::SM {
    mixin ::automata::Printer

    #: A simple sort of virtual Stack Machine.
    #: 
    #: The ID of a SM is (s, q) = current stack, current state.

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
        #: Inject the ExecStack method into T.
        oo::objdefine T method ExecStack id {
            lassign $id stack q0
            lassign [lindex [my get $q0 *] 0] - op addr val
            switch $op {
                INC {
                    lset stack 0 [expr {[lindex $stack 0] + 1}]
                    set q1 [expr {$q0 + 1}]
                }
                DEC {
                    lset stack 0 [expr {[lindex $stack 0] - 1}]
                    set q1 [expr {$q0 + 1}]
                }
                JZ {
                    if {[lindex $stack 0] eq 0} {
                        set q1 $addr
                    } else {
                        set q1 [expr {$q0 + 1}]
                    }
                }
                JE {
                    lassign [lrange $stack 0 1] v0 v1
                    if {$v0 eq $v1} {
                        set q1 $addr
                    } else {
                        set q1 [expr {$q0 + 1}]
                    }
                }
                J {
                    set q1 $addr
                }
                CLR {
                    set stack {}
                    set q1 [expr {$q0 + 1}]
                }
                DUP {
                    set stack [linsert $stack 0 [lindex $stack 0]]
                    set q1 [expr {$q0 + 1}]
                }
                PUSH {
                    set stack [linsert $stack 0 $val]
                    set q1 [expr {$q0 + 1}]
                }
                ADD {
                    lassign [lrange $stack 0 1] v0 v1
                    set stack [lreplace $stack 0 1 [expr {$v0 + $v1}]]
                    set q1 [expr {$q0 + 1}]
                }
                MUL {
                    lassign [lrange $stack 0 1] v0 v1
                    set stack [lreplace $stack 0 1 [expr {$v0 * $v1}]]
                    set q1 [expr {$q0 + 1}]
                }
                HALT {
                    return
                }
                default {
                    error \$op=$op
                }
            }
            my addNewIDs [list $stack $q1]
        }

    }

    method compile tokens {
        #: Convert source code to transition configuration.
        set i 0
        set labels {}
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set labels [string trimright $token :] $i
                continue
            }
            regexp {([[:upper:]]+):?(\d*),?(\w*)$} $token -> op val offset
            if {$offset eq {}} {
                set offset 0
            }
            T set $i $op $offset $val
            incr i
        }
        my Q clear
        my Q set {*}[my T fixJumps $labels]
    }

    method run {stack {s {}}} {
        #: Run the code with the given register settings, starting from s.
        if {$s ne {}} {
            my S set $s
        }
        set ids [list [list $stack [my S get]]]
        set results [my T iterate $ids ExecStack]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `T` : public methods to give access to the components.
}
