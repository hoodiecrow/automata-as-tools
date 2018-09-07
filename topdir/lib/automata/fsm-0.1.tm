
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::fa
package require automata::ste

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::fa

    variable data

    constructor args {
        lassign $args data
        set data [dict merge {A {} B {} Q {} S {} F {}} $data]
        ::automata::STE create T
    }

    forward T T
    export T

    method AddState {key state} {
        # Add the state symbol to Q and optionally to F or S.
        dict with data {
            if {$key in {F S}} {
                set Q [lsort -unique [list {*}$Q $state]]
            }
            set $key [lsort -unique [list {*}[set $key] $state]]
        }
        return
    }
    
    method AddSymbols {key args} {
        # Add one or more non-empty symbols to (key).
        dict with data {
            foreach arg $args {
                if {$arg ne {}} {
                    lappend $key $arg
                }
            }
            set $key [lsort -unique [set $key]]
        }
    }

    method GetTarget {state symbol} {
        # Query the STE for a state to move to.
        my T getTransitions $state $symbol
    }

    method SetTarget {fromstate symbol tostate args} {
        # Add a new transition to the STE, and put the states
        # and the symbols in their boxes.
        my AddState Q $fromstate
        my AddSymbols A $symbol
        my AddState Q $tostate
        my AddSymbols B {*}$args
        my T add $fromstate $symbol $tostate $args
    }

    method IsIn? {key value} {
        # Does the value exist in the key?
        # (Or, is the key set to '*'?)
        dict with data {
            set values [set $key]
            expr {([llength $values] == 1 && [lindex $values 0] eq "*") || $value in $values}
        }
    }

    method IsInMultiple? {key valueset} {
        # Does at least one of the values exist in the key?
        dict with data {
            set values [set $key]
            foreach value $valueset {
                if {$value in $values} {
                    return 1
                }
            }
            return 0
        }
    }

    method get args {
        # assume that things exist
        dict get $data {*}$args
    }

    # Finally, the actual behavior of the machine

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

    method classify {a b} {
        # What state are we in when all input symbols in a and b are consumed?
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s $b $f Consume Consume]
        return [lmap result $results {
            index $result 1
        }]
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
