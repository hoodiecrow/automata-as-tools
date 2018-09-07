
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::fsa

namespace eval automata {}

oo::class create ::automata::FST {
    mixin ::automata::FSA

    variable data

    constructor args {
        lassign $args data
        set data [dict merge {A {} B {} Q {} S {} F {}} $data]
    }

    method recognize {a b} {
        # Are we in a final state when all input symbols in a and b are consumed?
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s $b $f Consume Consume]
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
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s {} $f Consume Produce]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct b {
        # What symbols have been added to a when all input symbols in b are consumed?
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate {} $s $b $f Produce Consume]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        # If we take N steps into the transition sequence (or sequence powerset), what to we get in a and b?
        set s [dict get $data S]
        set f [dict get $data F]
        my T iterate -steps $steps {} $s {} $f Produce Produce
    }

}
