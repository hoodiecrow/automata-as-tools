namespace eval automata {}

oo::class create ::automata::Transducer {

    method recognize id {
        # unpack ID
        lassign $id a q0 b
        set _a [lassign $a A]
        set _b [lassign $b B]
        # get epsilons
        set tuples [my get $q0 {}]
        # get moves
        lappend tuples {*}[my get $q0 $A]
        my addNewIDs {*}[lmap tuple $tuples {
            # q1 from tuple
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                set tuple [lreplace $tuple 0 1 $a]
            } else {
                # consume input token
                set tuple [lreplace $tuple 0 1 $_a]
            }
            if {$out eq {}} {
                lset tuple 2 $b
            } elseif {$out ne $B} {
                # reject invalid transition
                continue
            } else {
                # consume output token
                lset tuple 2 $_b
            }
        }]
    }

    method translate id {
        # unpack ID
        lassign $id a q0 b
        set _a [lassign $a A]
        # get epsilons
        set tuples [my get $q0 {}]
        # get moves
        lappend tuples {*}[my get $q0 $A]
        my addNewIDs {*}[lmap tuple $tuples {
            # q1 from tuple
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                set tuple [lreplace $tuple 0 1 $a]
            } else {
                # consume input token
                set tuple [lreplace $tuple 0 1 $_a]
            }
            if {$out eq {}} {
                lset tuple 2 $b
            } else {
                # emit output token
                lset tuple 2 [linsert $b end [lindex $out 0]]
            }
        }]
    }

    method reconstruct id {
        # unpack ID
        lassign $id a q0 b
        set _b [lassign $b B]
        # get moves
        set tuples [my get $q0 *]
        my addNewIDs {*}[lmap tuple $tuples {
            # q1 from tuple
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                set tuple [lreplace $tuple 0 1 $a]
            } else {
                # emit input token
                set tuple [lreplace $tuple 0 1 [linsert $a end [lindex $inp 0]]]
            }
            if {$out eq {}} {
                lset tuple 2 $b
            } elseif {$out ne $B} {
                # reject invalid transition
                continue
            } else {
                # consume output token
                lset tuple 2 $_b
            }
        }]
    }

    method generate id {
        # unpack ID
        lassign $id a q0 b
        # get moves
        set tuples [my get $q0 *]
        my addNewIDs {*}[lmap tuple $tuples {
            # q1 from tuple
            lassign $tuple - inp q1 out
            if {$inp eq {}} {
                set tuple [lreplace $tuple 0 1 $a]
            } else {
                # emit input token
                set tuple [lreplace $tuple 0 1 [linsert $a end [lindex $inp 0]]]
            }
            if {$out eq {}} {
                lset tuple 2 $b
            } else {
                # emit output token
                lset tuple 2 [linsert $b end [lindex $out 0]]
            }
        }]
    }

}
