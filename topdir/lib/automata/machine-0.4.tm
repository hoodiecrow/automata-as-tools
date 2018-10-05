namespace eval automata {}

oo::class create ::automata::Machine {
    variable iddef

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
            set targets [lmap row [my get table $state {}] {
                lindex $row 2
            }]
            set ids [lmap target $targets {
                $iddef make $input $target
            }]
            set _tail [lassign $input top]
            set targets [lmap row [my get table $state $top] {
                lindex $row 2
            }]
            lappend ids {*}[lmap target $targets {
                $iddef make $_tail $target
            }]
        }
        return $ids
    }

    method recognize id {
        # unpack ID
        dict with id {
            # get epsilons
            set tuples [my get table $state {}]
            set itail [lassign $input itop]
            set otail [lassign $output otop]
            # get moves
            lappend tuples {*}[my get table $state $itop]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # consume input token
                    lset tuple 1 $itail
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } elseif {$out ne $otop} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $otail
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method translate id {
        # unpack ID
        dict with id {
            set itail [lassign $input itop]
            # get epsilons
            set tuples [my get table $state {}]
            # get moves
            lappend tuples {*}[my get table $state $itop]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # consume input token
                    lset tuple 1 $itail
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } else {
                    # emit output token
                    lset tuple 3 [linsert $output end [lindex $out 0]]
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method reconstruct id {
        # unpack ID
        dict with id {
            set otail [lassign $output otop]
            # get moves
            set tuples [my get table $state *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # emit input token
                    lset tuple 1 [linsert $input end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } elseif {$out ne $otop} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $otail
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method generate id {
        # unpack ID
        dict with id {
            # get moves
            set tuples [my get table $state *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # emit input token
                    lset tuple 1 [linsert $input end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } else {
                    # emit output token
                    lset tuple 3 [linsert $output end [lindex $out 0]]
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method PDA-exec id {
        # unpack ID
        dict with id {
            set itail [lassign $input itop]
            set _tail [lassign $stack _top]
            # get epsilons
            set tuples [my get table $state {}]
            # get moves
            lappend tuples {*}[my get table $state $itop]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 O _o
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # consume input token
                    lset tuple 1 $itail
                }
                if {$O ne $_top} {
                    # reject invalid transition
                    continue
                } else {
                    # push stack
                    lset tuple 3 [concat {*}$_o $_tail]
                }
                # TODO ??
                $iddef make {*}[apply {tuple {
                    set _tail [lassign $tuple - input state stack]
                    list $input $state $stack
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

    method Print {varName h p} {
        upvar 1 $varName tape
        switch $p {
            N  {}
            E  { lset tape $h [lindex [my get A] 0] }
            P  { lset tape $h [lindex [my get A] 1] }
            default {
                if {[regexp {^P(.)$} $p -> s]} {
                    lset tape $h $s
                }
            }
        }
        return
    }

    method Move {varName1 varName2 dir} {
        upvar 1 $varName1 tape $varName2 h
        switch $dir {
            L {
                incr h
                if {$h >= [expr {[llength $tape] - 1}]} {
                    lappend tape [lindex [my get A] 0]
                }
            }
            R {
                if {$h < 1} {
                    set tape [linsert $tape 0 [lindex [my get A] 0]]
                } else {
                    incr h -1
                }
            }
            N {}
        }
        return
    }

    method BTM-exec id {
        # unpack ID
        dict with id {
            if {[my in F $state]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [my get table $state [lindex $tape $head]]
            set ids [lmap tuple $tuples {
                lassign $tuple - - next print move
                my Print tape $head $print
                my Move tape head $move
                $iddef make $tape $head $next
            }]
        }
        return $ids
    }

    method ALU {op data args} {
        # Shared between KTR and SM
        switch $op {
            INC { set res [expr {[lindex $data {*}$args] + 1}] }
            DEC { set res [expr {[lindex $data {*}$args] - 1}] }
            CLR { set res 0 }
            default {
                if {[string is upper -strict $op]} {
                    set op [dict get {
                        EQ  eq
                        EQL ==
                        ADD +
                        MUL *
                    } $op]
                }
                set res [::tcl::mathop::$op {*}[lmap arg $args {
                    lindex $data $arg
                }]]
            }
        }
        if {$res < 0} {
            return -code error [format {result less than 0}]
        }
        return $res
    }

}
