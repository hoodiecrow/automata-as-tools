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
    #: The ID of a SM is (s, q, f) = current stack, current state, lookahead flag.
    #:
    #: The operations of the programming language are:
    #: 
    #: INC, DEC:r : increment/decrement contents of register r
    #: CLR:r      : set contents of register r to 0
    #: CPY:r0 r1  : copy contents of register r0 to r1
    #: op:r0 r1 r2: (op = EQ, EQL, ADD, MUL) perform ALU op on r0 and r1 and stores the result in r2
    #: JZ:r,label : jump to label if contents of register r = 0
    #: HALT       : stop the program

    constructor args {
        #: This machine is defined by the tuple `<A, Q, S, T>`:
        ::automata::Component create A -label "Flag symbols" -domain B
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
            regexp {([[:upper:]]+):?(\w*)$} $token -> op val
            set next [expr {$i + 1}]
            if {$op eq "JZ"} {
                T set $i 0 $val $op - 0 1
                T set $i 1 $next $op - 0 1
            } elseif {$op eq "J"} {
                T set $i [A set] $val $op - 0 1
            } else {
                T set $i [A set] $next $op $val 0 1
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
        set ids [list [list $stack [my S get] [expr {[lindex $stack 0] != 0}]]]
        set results [my T iterate $ids ExecStack]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `T` : public methods to give access to the components.
}
