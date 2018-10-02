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

    method PTM-exec id {
        # unpack ID
        dict with id {
            if {[my in F $q]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [my get table $q [lindex $t $h]]
            if {[llength $tuples] eq 0} {
                return
            }
            set tuple [lindex $tuples 0]
            lassign $tuple - - q code
            lassign $code tag a b
            switch $tag {
                HALT  {
                    return
                }
                PRINT: {
                    lset t $h [lindex [my get A] $a]
                }
                ROLL: {
                    # PTM has reversed sense of movement
                    my Move t h [string map {R L L R} $a]
                }
                NOP {}
            }
            set ids [list [my add id $t $h $q]]
        }
        return $ids
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
                lassign $tuple - - q1 p m
                my Print t $h $p
                my Move t h $m
                my add id $t $h $q1
            }]
        }
        return $ids
    }

    method CM-exec id {
        # unpack ID
        dict with id {
            if {[my in F $i]} {
                return
            }
            # get move
            lassign [lindex [my get table $i 0] 0] - - - code
            lassign $code tag a b
            switch $tag {
                JE: { set flag [expr [lindex $r $a] eq [lindex $r $b]] }
                JZ: { set flag [expr [lindex $r $a] eq [lindex $r 0]] }
                default {
                    set flag 0
                }
            }
            lassign [lindex [my get table $i $flag] 0] - - i
            # build new ID
            switch $tag {
                INC: { lset r $a [expr {[lindex $r $a] + 1}] }
                DEC: { lset r $a [expr {[lindex $r $a] - 1}] }
                CLR: { lset r $a [lindex $r 0] }
                CPY: { lset r $a [lindex $r $b] }
                NOP  {}
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

    method SM-exec id {
        # unpack ID
        dict with id {
            if {[my in F $i]} {
                return
            }
            lassign [lindex [my get table $i 0] 0] - - - code
            lassign $code tag a b c d e f
            lassign $s TOP
            switch $tag {
                JSZ: {
                    set flag [expr {$TOP == 0}]
                }
                JSE: {
                    set flag [expr {$TOP == [lindex $s 1]}]
                }
                default {
                    set flag 0
                }
            }
            lassign [lindex [my get table $i $flag] 0] - - i
            # get move
            switch $tag {
                PUSH {
                    set s [linsert $s 0 $a]
                }
                INC - DEC - CLR {
                    lset s 0 [my ALU $tag $s 0]
                }
                DUP {
                    set s [linsert $s 0 $TOP]
                }
                EQ - EQL - ADD - MUL {
                    set v [my ALU $tag $s 0 1]
                    set s [lreplace $s 0 1 $v]
                }
            }
            if {[lindex $s 0] < 0} {
                return -code error [format {negative value in top of stack}]
            }
            # build new ID
            list [my add id $s $i]
        }
    }

    method Turn {varName {a 1}} {
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
        set _a [list 0 $y [expr {$w + 1}] $y $x 0 $x [expr {$h + 1}]]
        foreach {X Y} [concat $_a $a] {
            if {$X eq $x && $Y eq $y} {return 1}
        }
        return 0
    }

    method Test {id idx} {
        dict with id {
            set label [lindex {
                front-is-clear
                left-is-clear
                right-is-clear
                next-to-a-beeper
                facing-north
                facing-south
                facing-east
                facing-west
                any-beepers-in-beeper-bag
            } $idx]
            switch $label {
                front-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f] $a]}
                }
                left-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f +1] $a]}
                }
                right-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f -1] $a]}
                }
                next-to-a-beeper { my FindBeeper $x $y $b }
                facing-east  { expr {$f eq 0} }
                facing-north { expr {$f eq 1} }
                facing-west  { expr {$f eq 2} }
                facing-south { expr {$f eq 3} }
                any-beepers-in-beeper-bag { expr {$n > 0} }
            }
        }
    }

    method KTR-exec id {
        # unpack ID
        dict with id {
            # get move
            lassign $i q
            lassign [lindex [my get table $q 0] 0] - - - code
            lassign $code tag _a _b _c _d _e
            # test-sensitive jumps are coded as NOP
            if {$tag eq "NOP"} {
                set flag $t
            } else {
                set flag 0
            }
            set t 0
            lassign [lindex [my get table $q $flag] 0] - - q
            switch $tag {
                JT: - NOP {
                    lset i 0 $q
                }
                HALT  {
                    return
                }
                TURN  {
                    my Turn f
                    lset i 0 $q
                }
                MOVE  {
                    my RMove $w $h x y $f $a
                    lset i 0 $q
                }
                TAKE - DROP {
                    lset i 0 $q
                }
                TEST: {
                    set t [my Test $id $_a]
                    lset i 0 $q
                }
                RET   {
                    set i [lrange $i 1 end]
                }
                CALL: {
                    lset i 0 [my succ Q [lindex $i 0]]
                    set i [linsert $i 0 $q]
                    set t 0
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
