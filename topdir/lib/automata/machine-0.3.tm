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

    method ExecCounter id {
        log::log d [info level 0] 
        # unpack ID
        dict with id {
            if {[my in F $i]} {
                return
            }
            # get move
            lassign [lindex [my get table $i *] 0] - - - - regs
            lassign $regs r_ r0
            set f [expr {[lindex $r $r_] == [lindex $r $r0]}]
            log::log d "\$r_=$r_, \$r0=$r0, \$f=$f"
            lassign [lindex [my get table $i $f] 0] - - i1 op regs
            lassign $regs - r0 r1 r2
            log::log d "[my get table $i $f]: \$i1=$i1 "
            switch $op {
                INC { lset r $r0 [expr {[lindex $r $r0] + 1}] }
                DEC { lset r $r0 [expr {[lindex $r $r0] - 1}] }
                CLR { lset r $r0 0 }
                CPY { lset r $r1 [lindex $r $r0] }
            }
            if {[lindex $r $r0] < 0} {
                return -code error [format {negative value in register %d} $r0]
            }
            if {[lindex $r 0] ne 0} {
                return -code error [format {register 0 has been changed}]
            }
            # build new ID
            list [my add id $r $i1]
        }
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
                eq - == - + - * {
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

    method Test {id index} {
        log::log d [info level 0] 
        dict with id {
            switch [my GetTestLabel $index] {
                front-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f] $a]}
                }
                front-is-blocked {
                    my CheckCollision $w $h {*}[my Look $x $y $f] $a
                }
                left-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f +1] $a]}
                }
                left-is-blocked {
                    my CheckCollision $w $h {*}[my Look $x $y $f +1] $a
                }
                right-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f -1] $a]}
                }
                right-is-blocked {
                    my CheckCollision $w $h {*}[my Look $x $y $f -1] $a
                }
                next-to-a-beeper { my FindBeeper $x $y $b }
                not-next-to-a-beeper { expr {![my FindBeeper $x $y $b]} }
                facing-north { expr {$f eq 1} }
                not-facing-north { expr {$f ne 1} }
                facing-south { expr {$f eq 3} }
                not-facing-south { expr {$f ne 3} }
                facing-east { expr {$f eq 0} }
                not-facing-east { expr {$f ne 0} }
                facing-west { expr {$f eq 2} }
                not-facing-west { expr {$f ne 2} }
                any-beepers-in-beeper-bag { expr {$n > 0} }
                no-beepers-in-beeper-bag { expr {$n < 1} }
            }
        }
    }

    method exec id {
        log::log d [info level 0] 
        # unpack ID
        dict with id {
            # get move
            lassign $i q
            log::log d [lindex [my get table $q $t] 0]
            lassign [lindex [my get table $q $t] 0] - - i1 op index
            set t 0
            lset i 0 $i1
            log::log d \$i1=$i1 
            log::log d \$i=$i 
            switch $op {
                TURN { my Turn f }
                MOVE { my RMove $w $h x y $f $a }
                TAKE {}
                DROP {}
                TEST {
                    set t [my Test $id $index]
                    log::log d \$t=$t 
                }
                RET {
                    set i [lrange $i 1 end]
                }
                GOSUB {
                    lset i 0 [my succ Q $q]
            log::log d \$i=$i 
                    set i [linsert $i 0 $i1]
            log::log d \$i=$i 
                    set t 0
                }
                NOP {}
                default {
                    error \$op=$op
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
