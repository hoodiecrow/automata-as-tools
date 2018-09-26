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
        dict with id {
            set _w [lassign $w W]
            set _z [lassign $z Z]
            # get epsilons
            set tuples [my get table $q {}]
            # get moves
            lappend tuples {*}[my get table $q $W]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 O _o
                if {$inp eq {}} {
                    lset tuple 1 $w
                } else {
                    # consume input token
                    lset tuple 1 $_w
                }
                if {$O ne $Z} {
                    # reject invalid transition
                    continue
                } else {
                    # push stack
                    lset tuple 3 [concat {*}$_o $_z]
                }
                my add id {*}[apply {tuple {
                    set _z [lassign $tuple - w q z]
                    list $w $q $z
                }} $tuple]
            }]
        }
        return $ids
    }

    #: In tape machines, data is in a sequential-accessed sequence that can
    #: grow if new elements are added at the ends.
    #:
    #: The operations supported are:
    #:
    #: Print
    #:  <symbol> print symbol (including blank)
    #:  N        do not print
    #:
    #: Move
    #:  L        move tape one cell to the left
    #:  R        move tape one cell to the right
    #:  N        do not move tape
    #:
    #: In the Post-Turing machine, the head is moved instead, so the compiler
    #: emits R for L and L for R.

    method Print {varName h s} {
        upvar 1 $varName t
        if {$s ne "N"} {
            lset t $h $s
        }
        return
    }

    method Move {varName1 varName2 dir} {
        upvar 1 $varName1 t $varName2 h
        switch $dir {
            L {
                incr h
                if {$h >= [expr {[llength $t] - 1}]} {
                    lappend t [my get E]
                }
            }
            R {
                if {$h < 1} {
                    set t [linsert $t 0 [my get E]]
                } else {
                    incr h -1
                }
            }
            N {}
        }
        return
    }

    method process id {
        # unpack ID
        dict with id {
            if {[my in F $q]} {
                return
            }
            set tuples [my get table $q [lindex $t $h]]
            # should always be 0 or 1 tuples
            set ids [lmap tuple $tuples {
                set q1 [lindex $tuple 2]
                my Print t $h [lindex $tuple 3]
                my Move t h [lindex $tuple 4]
                my add id $t $h $q1
            }]
        }
        return $ids
    }

}
