oo::class create ::automata::Robot {
    variable w h x y f b q t beepers walls
    
    method Turn {{a 1}} {
        set f [expr {($f + $a + 4) % 4}]
    }

    method Move {} {
        switch $f {
            0 { set x [expr {$x + 1}] }
            1 { set y [expr {$y + 1}] }
            2 { set x [expr {$x - 1}] }
            3 { set y [expr {$y - 1}] }
        }
    }

    method Look {{ddir 0}} {
        set _x $x
        set _y $y
        switch [expr {($f + $ddir + 4) % 4}] {
            0 { set _x [expr {$x + 1}] }
            1 { set _y [expr {$y + 1}] }
            2 { set _x [expr {$x - 1}] }
            3 { set _y [expr {$y - 1}] }
        }
        return [list $_x $_y]
    }

    method FindBeeper {} {
        foreach {X Y n} $beepers {
            if {$X eq $x && $Y eq $y} {
                return 1
            }
        }
        return 0
    }

    method CheckCollision {_x _y} {
        log::log d [info level 0] 
        set _walls [list 0 $_y [expr {$w + 1}] $_y $_x 0 $_x [expr {$h + 1}]]
        foreach {X Y} [concat $walls $_walls] {
            if {$X eq $_x && $Y eq $_y} {return 1}
        }
        return 0
    }

    method exec id {
        log::log d [info level 0] 
        # unpack ID
        lassign $id world robot q t beepers walls
        lassign $world w h
        lassign $robot x y f b
        lassign $q q0
        # get move
        lassign [lindex [my get $q0 *] 0] - op addr label
        log::log d "\$op=$op \$label=$label"
        switch $op {
            A {
                switch $label {
                    turnleft {
                        my Turn
                    }
                    move {
                        my Move
                        if {[my CheckCollision $x $y]} {
                            return -code error [format {collision with a wall!}]
                        }
                    }
                    pickbeeper {}
                    putbeeper {}
                    turnoff {
                        return
                    }
                    default {
                        error \$label=$label
                    }
                }
                lset q 0 [my Q succ $q0]
                set t {}
            }
            T {
                set _walls [concat $walls [list 0 $y [expr {$w + 1}] $y $x 0 $x [expr {$h + 1}]]]
                switch $label {
                    front-is-clear {
                        set t [expr {![my CheckCollision {*}[my Look]]}]
                    }
                    front-is-blocked {
                        set t [my CheckCollision {*}[my Look]]
                    }
                    left-is-clear {
                        set t [expr {![my CheckCollision {*}[my Look +1]]}]
                    }
                    left-is-blocked {
                        set t [my CheckCollision {*}[my Look +1]]
                    }
                    right-is-clear {
                        set t [expr {![my CheckCollision {*}[my Look -1]]}]
                    }
                    right-is-blocked {
                        set t [my CheckCollision {*}[my Look -1]]
                    }
                    next-to-a-beeper { set t [my FindBeeper] }
                    not-next-to-a-beeper { set t [expr {![my FindBeeper]}] }
                    facing-north { set t [expr {$f eq 1}] }
                    not-facing-north { set t [expr {$f ne 1}] }
                    facing-south { set t [expr {$f eq 3}] }
                    not-facing-south { set t [expr {$f ne 3}] }
                    facing-east { set t [expr {$f eq 0}] }
                    not-facing-east { set t [expr {$f ne 0}] }
                    facing-west { set t [expr {$f eq 2}] }
                    not-facing-west { set t [expr {$f ne 2}] }
                    any-beepers-in-beeper-bag { set t [expr {$b > 0}] }
                    no-beepers-in-beeper-bag { set t [expr {$b < 1}] }
                    default {
                        error "label = $label"
                    }
                }
                lset q 0 [my Q succ $q0]
            }
            JZ {
                set v [expr {$t eq 0}]
                lset q 0 [if {$v} {set addr} {my Q succ $q0}]
                set t {}
            }
            JNZ {
                set v [expr {$t ne 0}]
                lset q 0 [if {$v} {set addr} {my Q succ $q0}]
                set t {}
            }
            J {
                lset q 0 $addr
                set t {}
            }
            RET {
                set q [lrange $q 1 end]
                set t {}
            }
            GOSUB {
                lset q 0 [my Q succ $q0]
                set q [linsert $q 0 $addr]
                set t {}
            }
            default {
                error \$op=$op
            }
        }
        # build new ID
        lappend _ids $world
        lappend _ids [list $x $y $f $b]
        lappend _ids $q
        lappend _ids $t
        lappend _ids $beepers
        lappend _ids $walls
        return [list $_ids]
    }
}

