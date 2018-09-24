::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::processor
package require automata::robot

namespace eval automata {}

oo::class create ::automata::KTR {
    mixin ::automata::Printer

    #: This is a very limited Karel the Robot that can only walk around, not
    #: interact with beepers.
    #:
    #: The ID of a KTR is (o, r, i, t, b, a) = world, robot, instruction pointer, teststate, beepers, walls

    constructor args {
        #: This machine is defined by the tuple `<A, Q, S, T>`:
        #:
        ::automata::Component create A -label "Operations used" -exclude {{}}
        ::automata::Component create Q -label "State symbols" -domain N
        ::automata::Component create S -label "Program start" -in [namespace which Q] -scalar
        S set 0
        ::automata::STE create T {Q S F A}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the Robot class into T.
        oo::objdefine T mixin -append ::automata::Robot

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
            regexp {([[:upper:]]+):?(.*)$} $token -> op label
            if {$label eq {}} {
                set label 0
            }
            if {$op in {T A}} {
                T set $i $op 0 $label
            } else {
                T set $i $op $label
            }
            incr i
        }
        my Q clear
        my Q set {*}[my T fixJumps $labels]
    }

    method run {world robot beepers walls {s {}}} {
        #: Run the code with the given configuration, starting from s.
        if {$s eq {}} {
            set s [my S get]
        }
        lappend id $world
        lappend id $robot
        lappend id $s
        lappend id {}
        lappend id $beepers
        lappend id $walls
        set results [my T iterate [list $id] exec]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `T` : public methods to give access to the components.
}
