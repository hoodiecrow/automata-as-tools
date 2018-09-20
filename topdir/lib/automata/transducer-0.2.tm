namespace eval automata {}

oo::class create ::automata::Transducer {

    method recognize id {
        # unpack ID
        lassign $id a q0 b
        set _a [lassign $a A]
        set _b [lassign $b B]
        # get epsilons
        set tuples1 [my get $q0 {}]
        # get moves
        set tuples2 [my get $q0 $A]
        set id [list]
        # build new IDs
        foreach tuple [concat $tuples1 $tuples2] {
            # q1 from tuple
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                lset id 0 $a
            } else {
                # consume input token
                lset id 0 $_a
            }
            lset id 1 $q1
            if {$out eq {}} {
                lset id 2 $b
            } elseif {$out ne $B} {
                # reject invalid transition
                continue
            } else {
                # consume output token
                lset id 2 $_b
            }
            my addNewIDs $id
        }
    }

    method translate id {
        # unpack ID
        lassign $id a q0 b
        set _a [lassign $a A]
        # get epsilons
        set tuples1 [my get $q0 {}]
        # get moves
        set tuples2 [my get $q0 $A]
        set id [list]
        # build new IDs
        foreach tuple [concat $tuples1 $tuples2] {
            # q1 from tuple
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                lset id 0 $a
            } else {
                # consume input token
                lset id 0 $_a
            }
            lset id 1 $q1
            if {$out ne {}} {
                # emit output token
                lset id 2 [list {*}$b [lindex $out 0]]
            } else {
                lset id 2 $b
            }
            my addNewIDs $id
        }
    }

    method reconstruct id {
        # unpack ID
        lassign $id a q0 b
        set _b [lassign $b B]
        set id [list]
        # get edges, build new IDs
        foreach tuple [my getEdges $q0] {
            # q1 from tuple
            lassign $tuple inp q1 out
            if {$inp ne {}} {
                # emit input token
                lset id 0 [list {*}$a [lindex $inp 0]]
            } else {
                lset id 0 $a
            }
            lset id 1 $q1
            if {$out eq {}} {
                lset id 2 $b
            } elseif {$out ne $B} {
                # reject invalid transition
                continue
            } else {
                # consume output token
                lset id 2 $_b
            }
            my addNewIDs $id
        }
    }

    method generate id {
        # unpack ID
        lassign $id a q0 b
        set id [list]
        # get edges, build new IDs
        foreach tuple [my getEdges $q0] {
            # q1 from tuple
            lassign $tuple inp q1 out
            if {$inp ne {}} {
                # emit input token
                lset id 0 [list {*}$a [lindex $inp 0]]
            } else {
                lset id 0 $a
            }
            lset id 1 $q1
            if {$out ne {}} {
                # emit output token
                lset id 2 [list {*}$b [lindex $out 0]]
            } else {
                lset id 2 $b
            }
            my addNewIDs $id
        }
    }

}
