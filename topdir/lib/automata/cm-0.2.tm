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
    #: The ID of a CM is (r, s, f) = current registers, current state, lookahead flag.

    constructor args {
        #: This machine is defined by the tuple `<A, Q, S, T>`:
        ::automata::Component create A -label "Flag symbols" -domain B
        ::automata::Component create Q -label "Instructions" -domain N
        ::automata::Component create S -label "Program start" -in [namespace which Q] -scalar
        ::automata::Component create F -label "Program end" -in [namespace which Q] -scalar
        ::automata::STE create T {Q A}
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
            # instruction set, after Shepherdson and Sturgis (1963); adds NOP to have anything to jump to when jumping to end.
            switch $op {
                INC - DEC - CLR - CPY {
                    T set $i [A set] $next $op {*}$regs
                }
                J {
                    T set $i [A set] $offset
                }
                JZ {
                    T set $i 0 $offset {} {*}$regs
                    T set $i 1 $next {} {*}$regs
                }
                NOP {
                    T set $i [A set] $next
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
        set ids [list [list $regs [my S get] $f]]
        set results [my T iterate $ids ExecCounter]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `T` : public methods to give access to the components.
}
