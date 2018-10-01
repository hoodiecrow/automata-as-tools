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
                my add id $a $target
            }]
            set _a [lassign $a A]
            set targets [lmap row [my get table $q $A] {
                lindex $row 2
            }]
            lappend ids {*}[lmap target $targets {
                my add id $_a $target
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

    method Print {varName h p} {
        log::log d [info level 0] 
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

    method process id {
        # unpack ID
        dict with id {
            if {[my in F $q]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [my get table $q [lindex $t $h]]
            set ids [lmap tuple $tuples {
                log::log d \$tuple=$tuple 
                lassign $tuple - - q1 p m
                log::log d \$p=$p 
                my Print t $h $p
                my Move t h $m
                my add id $t $h $q1
            }]

        }
        return $ids
    }

    method ExecCounter id {
        # unpack ID
        dict with id {
            if {[my in F $i]} {
                return
            }
            # get move
            set tuples [my get table $i *]
            lassign [lindex $tuples 0] - - - O
            lassign $O tag a b c
            if {$tag eq "je"} {
                set flag [expr [lindex $r $b] eq [lindex $r $c]]
            } else {
                set flag 0
            }
            lassign [lindex $tuples $flag] - - i
            # build new ID
            switch $tag {
                halt { return }
                inc { lset r $a [expr {[lindex $r $a] + 1}] }
                dec { lset r $a [expr {[lindex $r $a] - 1}] }
                set { lset r $a [lindex $r $b] }
                nop {}
            }
            if {[lindex $r 0] ne 0} {
                return -code error [format {register 0 has been changed}]
            }
            set res [list [my add id $r $i]]
        }
        return $res
    }

    method ALU {op data args} {
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

    method ExecStack id {
        log::log d [info level 0] 
        # unpack ID
        dict with id {
            if {[my in F $i]} {
                return
            }
            lassign $s TOP
            # get move
            set flag [expr {$TOP != 0}]
            lassign [lindex [my get table $i $flag] 0] - - i1 op val
            switch $op {
                PUSH {
                    set s [linsert $s 0 $val]
                }
                INC - DEC - CLR {
                    lset s 0 [my ALU $op $s 0]
                }
                DUP {
                    set s [linsert $s 0 $TOP]
                }
                EQ - EQL - ADD - MUL {
                    set v [my ALU $op $s 0 1]
                    set s [lreplace $s 0 1 $v]
                }
            }
            if {[lindex $s 0] < 0} {
                return -code error [format {negative value in top of stack}]
            }
            # build new ID
            list [my add id $s $i1]
        }
    }

    method Turn {varName {a 1}} {
        log::log d [info level 0] 
        upvar 1 $varName f
        set f [expr {($f + $a + 4) % 4}]
    }

    method RMove {w h varName1 varName2 f a} {
        upvar 1 $varName1 x $varName2 y
        switch $f {
            0 { set x [expr {$x + 1}] }
            1 { set y [expr {$y + 1}] }
            2 { set x [expr {$x - 1}] }
            3 { set y [expr {$y - 1}] }
        }
        if {[my CheckCollision $w $h $x $y $a]} {
            return -code error [format {collision with a wall!}]
        }
    }

    method Look {x y f {ddir 0}} {
        switch [expr {($f + $ddir + 4) % 4}] {
            0 { incr x }
            1 { incr y }
            2 { incr x -1 }
            3 { incr y -1 }
        }
        return [list $x $y]
    }

    method FindBeeper {x y b} {
        foreach {X Y} $b {
            if {$X eq $x && $Y eq $y} {
                return 1
            }
        }
        return 0
    }

    method CheckCollision {w h x y a} {
        log::log d [info level 0] 
        set _a [list 0 $y [expr {$w + 1}] $y $x 0 $x [expr {$h + 1}]]
        foreach {X Y} [concat $_a $a] {
            if {$X eq $x && $Y eq $y} {return 1}
        }
        return 0
    }

    method Test {id _a _b} {
        log::log d [info level 0] 
        dict with id {
            switch [list $_a $_b] {
                {2 0} {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f] $a]}
                }
                {2 1} {
                    my CheckCollision $w $h {*}[my Look $x $y $f] $a
                }
                {3 0} {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f +1] $a]}
                }
                {3 1} {
                    my CheckCollision $w $h {*}[my Look $x $y $f +1] $a
                }
                {4 0} {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f -1] $a]}
                }
                {4 1} {
                    my CheckCollision $w $h {*}[my Look $x $y $f -1] $a
                }
                {5 0} { my FindBeeper $x $y $b }
                {6 0} { expr {![my FindBeeper $x $y $b]} }
                {0 1} { expr {$f eq 1} }
                {1 1} { expr {$f ne 1} }
                {0 3} { expr {$f eq 3} }
                {1 3} { expr {$f ne 3} }
                {0 0} { expr {$f eq 0} }
                {1 0} { expr {$f ne 0} }
                {0 2} { expr {$f eq 2} }
                {1 2} { expr {$f ne 2} }
                {7 0} { expr {$n > 0} }
                {8 0} { expr {$n < 1} }
            }
        }
    }

    method exec id {
        log::log d [info level 0] 
        # unpack ID
        dict with id {
            # get move
            lassign $i q
            set tuples [my get table $q *]
            lassign [lindex $tuples 0] - - - O
            lassign $O tag _a _b _c
            switch $tag {
                je { set t [expr {$_b eq $_c}] }
            }
            lassign [lindex $tuples $t] - - i1
            set t 0
            lset i 0 $i1
            switch $tag {
                halt { return }
                turn { my Turn f}
                move { my RMove $w $h x y $f $a }
                take {}
                drop {}
                test { set t [my Test $id $_a $_b] }
                ret  { set i [lrange $i 1 end] }
                call {
                    lset i 0 [my succ Q $q]
                    set i [linsert $i 0 $i1]
                    set t 0
                }
                je   {}
                jt   {}
                nop  {}
                default {
                    error \$tag=$tag
                }
            }
        }
        if {[my in F [lindex $i 0]]} {
            return
        }
        # build new ID
        lappend _id $w $h
        lappend _id $x $y $n $f
        lappend _id $i
        lappend _id $t
        lappend _id $b
        lappend _id $a
        return [list [my add id {*}$_id]]
    }

}
