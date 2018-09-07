
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
        dict with data {
            if {$key ne "Q"} {
                set Q [lsort -unique [list {*}$Q $state]]
            }
            set $key [lsort -unique [list {*}[set $key] $state]]
        }
        return
    }
    
    method AddSymbols {key args} {
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
        my T getTransitions $state $symbol
    }

    method SetTarget {fromstate symbol tostate args} {
        my AddState Q $fromstate
        my AddSymbols A $symbol
        my AddState Q $tostate
        my AddSymbols B {*}$args
        my T add $fromstate $symbol $tostate $args
    }

    method IsEpsilonFree {} {
        my T isEpsilonFree
    }

    method IsDeterministic {} {
        my T isDeterministic
    }

    method IsIn? {key state} {
        dict with data {
            set states [set $key]
            expr {([llength $states] == 1 && [lindex $states 0] eq "*") || $state in $states}
        }
    }

    method IsInMultiple? {key stateset} {
        dict with data {
            set states [set $key]
            foreach state $stateset {
                if {$state in $states} {
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

    method accept a {
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s {} $f Consume NullTape]
        foreach result $results {
            lassign $result a
            if {[llength $a] == 0} {
                return 1
            }
        }
        return 0
    }

    method classify {a b} {
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s $b $f Consume Consume]
        return [lmap result $results {
            index $result 1
        }]
    }

    method recognize {a b} {
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
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate $a $s {} $f Consume Produce]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct b {
        set s [dict get $data S]
        set f [dict get $data F]
        set results [my T iterate {} $s $b $f Produce Consume]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        set s [dict get $data S]
        set f [dict get $data F]
        my T iterate -steps $steps {} $s {} $f Produce Produce
    }

}
