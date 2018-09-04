
namespace eval automata {}

oo::class create ::automata::TransitionList {
    variable data is

    constructor args {
        lassign $args data
        set is(epsilon-free) 1
        set is(deterministic) 1
    }

    method add {q0 s q1 v} {
        lappend data [list $q0 $s $q1 $v]
    }

    method getAllStates {} {
        lsort -unique [concat \
            [lmap item $data {lindex $item 0}] \
            [lmap item $data {lindex $item 2}]]
    }

    method getFromStates {} {
        lsort -unique [lmap item $data {lindex $item 0}]
    }

    method getSymbols {q0 {q1 *}} {
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 2 -inline $items $q1]
        lmap item $items {lindex $item 1}
    }

    method getEdges q0 {
        set items [lsearch -all -index 0 -inline $data $q0]
        lmap item $items {lrange $item 1 end}
    }

    method getTransitions {q0 s} {
        set items [lsearch -all -index 0 -inline $data $q0]
        set items [lsearch -all -index 1 -inline $items $s]
        lmap item $items {lrange $item 2 end}
    }

}
