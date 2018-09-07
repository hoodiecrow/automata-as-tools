
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::ste

oo::class create ::automata::FSA {
    variable data

    constructor args {
        ::automata::STE create T
        next {*}$args
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

    method lappend args {
        dict lappend data {*}$args
    }

}
