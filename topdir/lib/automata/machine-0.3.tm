namespace eval automata {}

oo::class create ::automata::Machine {

    constructor args {
        next {*}$args
    }

    method search {id fn {steps {}}} {
        if {$steps ne {}} {
            if {$steps <= 0} {
                return [list $id]
            } else {
                incr steps -1
            }
        }
        set ids [my $fn $id]
        if {[llength $ids] eq 0} {
            return [list $id]
        }
        set ids [lsort -unique $ids]
        return [concat {*}[lmap id $ids {
            my search $id $fn $steps
        }]]
    }

    method consumeOne id {
        # unpack ID
        dict with id {
            # get epsilons
            set targets [lmap row [my get table $q {}] {
                lindex $row 2
            }]
            set ids [lmap target $targets {
                my add id $w $target
            }]
            set _w [lassign $w W]
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
        dict with id {
            # get epsilons
            set tuples [my get table $q {}]
            set _a [lassign $a A]
            set _b [lassign $b B]
            # get moves
            lappend tuples {*}[my get table $q $A]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # consume input token
                    lset tuple 1 $_a
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } elseif {$out ne $B} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $_b
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method translate id {
        # unpack ID
        dict with id {
            set _a [lassign $a A]
            # get epsilons
            set tuples [my get table $q {}]
            # get moves
            lappend tuples {*}[my get table $q $A]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # consume input token
                    lset tuple 1 $_a
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } else {
                    # emit output token
                    lset tuple 3 [linsert $b end [lindex $out 0]]
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method reconstruct id {
        # unpack ID
        dict with id {
            set _b [lassign $b B]
            # get moves
            set tuples [my get table $q *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # emit input token
                    lset tuple 1 [linsert $a end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } elseif {$out ne $B} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $_b
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method generate id {
        # unpack ID
        dict with id {
            # get moves
            lappend tuples {*}[my get table $q *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # emit input token
                    lset tuple 1 [linsert $a end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } else {
                    # emit output token
                    lset tuple 3 [linsert $b end [lindex $out 0]]
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
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
