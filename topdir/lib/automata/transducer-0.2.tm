namespace eval automata {}

oo::class create ::automata::Transducer {

    method recognize id {
        lassign $id a q0 b
        set _a [lassign $a A]
        set _b [lassign $b B]
        set tuples1 [my get $q0 {}]
        set tuples2 [my get $q0 $A]
        set id [list]
        foreach tuple [concat $tuples1 $tuples2] {
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                lset id 0 $a
            } else {
                lset id 0 $_a
            }
            lset id 1 $q1
            if {$out eq {}} {
                lset id 2 $b
            } elseif {$out ne $B} {
                continue
            } else {
                lset id 2 $_b
            }
            my addNewIDs $id
        }
    }

    method translate id {
        lassign $id a q0 b
        set _a [lassign $a A]
        set tuples1 [my get $q0 {}]
        set tuples2 [my get $q0 $A]
        set id [list]
        foreach tuple [concat $tuples1 $tuples2] {
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                lset id 0 $a
            } else {
                lset id 0 $_a
            }
            lset id 1 $q1
            if {$out ne {}} {
                lset id 2 [list {*}$b [lindex $out 0]]
            } else {
                lset id 2 $b
            }
            my addNewIDs $id
        }
    }

    method reconstruct id {
        lassign $id a q0 b
        set _b [lassign $b B]
        set id [list]
        foreach tuple [my getEdges $q0] {
            lassign $tuple inp q1 out
            if {$inp ne {}} {
                lset id 0 [list {*}$a [lindex $inp 0]]
            } else {
                lset id 0 $a
            }
            lset id 1 $q1
            if {$out eq {}} {
                lset id 2 $b
            } elseif {$out ne $B} {
                continue
            } else {
                lset id 2 $_b
            }
            my addNewIDs $id
        }
    }

    method generate id {
        lassign $id a q0 b
        set id [list]
        foreach tuple [my getEdges $q0] {
            lassign $tuple inp q1 out
            if {$inp ne {}} {
                lset id 0 [list {*}$a [lindex $inp 0]]
            } else {
                lset id 0 $a
            }
            lset id 1 $q1
            if {$out ne {}} {
                lset id 2 [list {*}$b [lindex $out 0]]
            } else {
                lset id 2 $b
            }
            my addNewIDs $id
        }
    }

}
