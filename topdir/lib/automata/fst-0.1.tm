
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::ste
package require automata::component

namespace eval automata {}

oo::class create ::automata::FST {
    variable data

    constructor args {
        ::automata::Component create A -nonempty
        ::automata::Component create B -nonempty
        ::automata::Component create Q
        ::automata::Component create S -in [namespace which Q]
        ::automata::Component create F -in [namespace which Q]
        ::automata::STE create T [self namespace] {Q A B}
    }

    foreach m {A B Q S F T} {
        forward $m $m ; export $m
    }

    method recognize {a b} {
        # Are we in a final state when all input symbols in a and b are consumed?
        set results [my T iterate $a [my S get] $b [my F get] Consume Consume]
        foreach result $results {
            lassign $result a q b
            if {[llength $a] == 0 && [llength $b] == 0} {
                return 1
            }
        }
        return 0
    }

    method translate a {
        # What symbols have been added to b when all input symbols in a are consumed?
        set results [my T iterate $a [my S get] {} [my F get] Consume Produce]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct b {
        # What symbols have been added to a when all input symbols in b are consumed?
        set results [my T iterate {} [my S get] $b [my F get] Produce Consume]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        # If we take N steps into the transition sequence (or sequence powerset), what to we get in a and b?
        my T iterate -steps $steps {} [my S get] {} [my F get] Produce Produce
    }

}
