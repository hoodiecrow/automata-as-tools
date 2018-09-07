
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::fsa

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::FSA

    variable data

    constructor args {
        lassign $args data
        set data [dict merge {A {} Q {} S {} F {}} $data]
    }

    method accept a {
        # Are we in a final state when all input symbols are consumed?
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s {} $f Consume NoOp]
        foreach result $results {
            lassign $result a
            if {[llength $a] == 0} {
                return 1
            }
        }
        return 0
    }

    method classify a {
        # What state are we in when all input symbols are consumed?
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s {} $f Consume NoOp]
        foreach result $results {
            lassign $result a q
            if {[llength $a] == 0} {
                return $q
            }
        }
    }

}
