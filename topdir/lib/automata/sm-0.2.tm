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
    #: The ID of a SM is (s, q, z) = current stack, current state, zero flag (0, 1, {}).

    constructor args {
        #: This machine is defined by the tuple `<A, Q, S, T>`:
        ::automata::Component create A -label "Flag symbols" -exclude {{}}
        ::automata::Component create B -label "Flag values"
        B set 0 1 {}
        ::automata::Component create Q -label "Instructions"
        ::automata::Component create S -label "Start address" -in [namespace which Q] -scalar
        S set 0
        ::automata::STE create T {Q A}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the Processor class into T.
        oo::objdefine T mixin -append ::automata::Processor

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
            set next [expr {$i + 1}]
            if {$op eq "JZ"} {
                T set $i 0 $next $op $val
                T set $i 1 $offset $op $val
                T set $i {} $next $op $val
            } else {
                T set $i [B set] $next $op $val
            }
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
        set ids [list [list $stack [my S get] [expr {[lindex $stack 0] == 0}]]]
        set results [my T iterate $ids ExecStack]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `T` : public methods to give access to the components.
}
