namespace eval automata {}

oo::class create ::automata::Machine {

    constructor args {
        next {*}$args
    }

    method search {id fn} {
        dict with id {
            if {[llength $w] eq 0} {
                return [list $id]
            } else {
                set ids [my $fn $id]
                return [concat {*}[lmap id $ids {
                    my search $id $fn
                }]]
            }
        }
    }

    method consumeOne id {
        # unpack ID
        dict with id {
            set _w [lassign $w W]
            # get epsilons
            set targets [lmap row [my get table $q {}] {
                lindex $row 2
            }]
            set ids [lmap target $targets {
                my add id $w $target
            }]
            set targets [lmap row [my get table $q $W] {
                lindex $row 2
            }]
            lappend ids {*}[lmap target $targets {
                my add id $_w $target
            }]
        }
        return $ids
    }

    method recognize id {
        # unpack ID
        lassign $id a q0 b
        set _a [lassign $a A]
        set _b [lassign $b B]
        # get epsilons
        set tuples [my get $q0 {}]
        # get moves
        lappend tuples {*}[my get $q0 $A]
        return [lmap tuple $tuples {
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
        return [lmap tuple $tuples {
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
        return [lmap tuple $tuples {
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
        return [lmap tuple $tuples {
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

    method makeMoves id {
        # unpack ID
        lassign $id a q0 b
        set _a [lassign $a A]
        set _b [lassign $b B]
        # get epsilons
        set tuples [my get $q0 {}]
        # get moves
        lappend tuples {*}[my get $q0 $A]
        # build new IDs
        return [lmap tuple $tuples {
            # q1 from tuple
            set _o [lassign $tuple - inp q1 O]
            if {$inp eq {}} {
                set tuple [lreplace $tuple 0 1 $a]
            } else {
                # consume input token
                set tuple [lreplace $tuple 0 1 $_a]
            }
            if {$O ne $B} {
                # reject invalid transition
                continue
            } else {
                # push stack
                lset tuple 2 [concat $_o $_b]
            }
        }]
    }

}
