namespace eval automata {}

oo::class create ::automata::Processor {
    variable ns

    method tapeMoveR {varName1 varName2} {
        upvar 1 $varName1 t $varName2 h
        incr h
        if {$h >= [expr {[llength $t] - 1}]} {
            lappend t [my Blank]
        }
    }

    method tapeMoveL {varName1 varName2} {
        upvar 1 $varName1 t $varName2 h
        if {$h < 1} {
            set t [linsert $t 0 [my Blank]]
        } else {
            incr h -1
        }
    }

    forward tapeMoveN list

    method process id {
        # unpack ID
        lassign $id t q0 h
        set tuples [my get $q0 [lindex $t $h]]
        if {[llength $tuples] == 0} {
            return
        } elseif {[llength $tuples] > 1} {
            return -code error [format {non-determinism detected: (%s)} $tuples]
        }
        lassign $tuples tuple
        # q1 from tuple
        lassign $tuple - inp q1 out
        lassign $out osym move
        # print to tape
        if {$osym ne "N"} {
            lset t $h $osym
        }
        # move tape/head
        my tapeMove$move t h
        # build new ID
        my addNewIDs [list $t $q1 $h]
    }

    method ALU {op args} {
        switch $op {
            INC { set res [expr {[lindex $args 0] + 1}] }
            DEC { set res [expr {[lindex $args 0] - 1}] }
            default {
                if {[string is upper -strict $op]} {
                    set op [dict get {
                        EQ  eq
                        EQL ==
                        ADD +
                        MUL *
                    } $op]
                }
                set res [::tcl::mathop::$op {*}$args]
            }
        }
        if {$res < 0} {
            return -code error [format {result less than 0}]
        }
        return $res
    }

    method ExecStack id {
        # unpack ID
        lassign $id stack q0
        set inp [expr {[lindex $stack 0] != 0}]
        # get move
        lassign [lindex [my get $q0 $inp] 0] - - q1 ov
        lassign $ov op val
        switch $op {
            INC - DEC {
                set v [my ALU $op [lindex $stack 0]]
                lset stack 0 $v
            }
            JZ - J {}
            CLR {
                set stack {}
            }
            DUP {
                set stack [linsert $stack 0 [lindex $stack 0]]
            }
            DUP2 {
                set stack [linsert $stack 0 [lrange $stack 0 1]]
            }
            PUSH {
                set stack [linsert $stack 0 $val]
            }
            POP {
                set stack [lrange $stack 1 end]
            }
            EQ - EQL - ADD - MUL {
                set v [my ALU $op {*}[lrange $stack 0 1]]
                set stack [lreplace $stack 0 1 $v]
            }
            HALT {
                return
            }
            default {
                error \$op=$op
            }
        }
        # build new ID
        my addNewIDs [list $stack $q1]
    }

    method ExecCounter id {
        # unpack ID
        lassign $id regs q0 flag
        # get move
        lassign [lindex [my get $q0 $flag] 0] - - q1 or
        set rs [lassign $or op]
        lassign $rs r
        switch $op {
            INC - DEC {
                set v [my ALU $op [lindex $regs $r]]
                lset regs $r $v
            }
            JZ {}
            CLR {
                lset regs $r 0
            }
            CPY {
                lassign $rs r0 r1
                lset regs $r1 [lindex $regs $r0]
            }
            EQ - EQL - ADD - MUL {
                lassign $rs r0 r1 r2
                set v [my ALU $op [lindex $regs $r0] [lindex $regs $r1]]
                lset regs $r2 $v
            }
            HALT {
                return
            }
            default {
                error \$op=$op
            }
        }
        # build new ID
        set r [lindex [my get $q1 *] 0 3 1]
        set f [expr {[lindex $regs $r] != 0}]
        my addNewIDs [list $regs $q1 $f]
    }

}
