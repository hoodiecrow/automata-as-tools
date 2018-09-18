namespace eval automata {}

oo::class create ::automata::Transducer {

    method recognize id {
        lassign $id a q0 b
        set tuples1 [my get $q0 {}]
        set tuples2 [my get $q0 [lindex $a 0]]
        set tuples [concat $tuples1 $tuples2]
        set _a [lrange $a 1 end]
        set _b [lrange $b 1 end]
        set id [list]
        foreach tuple $tuples {
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                lset id 0 $a
            } else {
                lset id 0 $_a
            }
            lset id 1 $q1
            if {$out eq {}} {
                lset id 2 $b
            } elseif {$out ne [lindex $b 0]} {
                continue
            } else {
                lset id 2 $_b
            }
            my addNewIDs $id
        }
    }

    method translate id {
        lassign $id a q0 b
        set tuples1 [my get $q0 {}]
        set tuples2 [my get $q0 [lindex $a 0]]
        set tuples [concat $tuples1 $tuples2]
        set _a [lrange $a 1 end]
        set id [list]
        foreach tuple $tuples {
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
        set tuples [my getEdges $q0]
        foreach tuple $tuples {
            log::log d \$tuple=$tuple 
            lassign $tuple inp q1 out
            if {$inp ne {}} {
                lset id 0 [list {*}$a [lindex $inp 0]]
            } else {
                lset id 0 $a
            }
            lset id 1 $q1
            if {$out eq {}} {
                lset id 2 $b
            } elseif {$out ne [lindex $b 0]} {
                continue
            } else {
                lset id 2 [lrange $b 1 end]
            }
            my addNewIDs $id
        }
    }

    method generate id {
        lassign $id a q0 b
        set tuples [my getEdges $q0]
        foreach tuple $tuples {
            log::log d \$tuple=$tuple 
            lassign $tuple inp q1 out
            if {$inp ne {}} {
                set _a [list {*}$a [lindex $inp 0]]
            } else {
                set _a $a
            }
            if {$out ne {}} {
                set _b [list {*}$b [lindex $out 0]]
            } else {
                set _b $b
            }
            my addNewIDs [list $_a $q1 $_b]
        }
    }

}
