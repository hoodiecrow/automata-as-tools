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
        if {[my CheckCollision $x $y]} {
            return -code error [format {collision with a wall!}]
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
        if {$t eq {}} {
            set t 0
        }
        lassign [lindex [my get $q0 $t] 0] - - q1 op label
        set t {}
        lset q 0 $q1
        switch $op {
            TURN { my Turn }
            MOVE { my Move }
            TAKE {}
            DROP {}
            TEST {
                set _walls [concat $walls [list 0 $y [expr {$w + 1}] $y $x 0 $x [expr {$h + 1}]]]
                set t [switch $label {
                    front-is-clear {
                        expr {![my CheckCollision {*}[my Look]]}
                    }
                    front-is-blocked {
                        my CheckCollision {*}[my Look]]
                    }
                    left-is-clear {
                        expr {![my CheckCollision {*}[my Look +1]]}
                    }
                    left-is-blocked {
                        my CheckCollision {*}[my Look +1]
                    }
                    right-is-clear {
                        expr {![my CheckCollision {*}[my Look -1]]}
                    }
                    right-is-blocked {
                        my CheckCollision {*}[my Look -1]
                    }
                    next-to-a-beeper { my FindBeeper }
                    not-next-to-a-beeper { expr {![my FindBeeper]} }
                    facing-north { expr {$f eq 1} }
                    not-facing-north { expr {$f ne 1} }
                    facing-south { expr {$f eq 3} }
                    not-facing-south { expr {$f ne 3} }
                    facing-east { expr {$f eq 0} }
                    not-facing-east { expr {$f ne 0} }
                    facing-west { expr {$f eq 2} }
                    not-facing-west { expr {$f ne 2} }
                    any-beepers-in-beeper-bag { expr {$b > 0} }
                    no-beepers-in-beeper-bag { expr {$b < 1} }
                    default {
                        error "label = $label"
                    }
                }]
            }
            RET {
                set q [lrange $q 1 end]
            }
            GOSUB {
                lset q 0 [my Q succ $q0]
                set q [linsert $q 0 $q1]
                set t {}
            }
            {} {}
            default {
                error \$op=$op
            }
        }
        if {[my F contains [lindex $q 0]]} {
            return
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

