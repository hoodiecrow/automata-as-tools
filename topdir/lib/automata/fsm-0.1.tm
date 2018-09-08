
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::ste
package require automata::component

namespace eval automata {}

oo::class create ::automata::FSM {
    variable data

    constructor args {
        ::automata::Component create A -nonempty
        ::automata::Component create Q
        ::automata::Component create S -in [namespace which Q]
        ::automata::Component create F -in [namespace which Q]
        ::automata::STE create T [self namespace] {Q A}
    }

    foreach m {A Q S F T} {
        forward $m $m ; export $m
    }

    method accept a {
        # Are we in a final state when all input symbols are consumed?
        set results [my T iterate $a [my S get] {} [my F get] Consume NoOp]
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
        set results [my T iterate $a [my S get] {} [my F get] Consume NoOp]
        if {[llength $a] == 0} {
            return [lmap result $results {lindex $result 1}]
        } else {
            return {}
        }
    }

}
