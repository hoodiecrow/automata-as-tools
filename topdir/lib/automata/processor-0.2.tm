namespace eval automata {}

oo::class create ::automata::Processor {

    method ALU {op data args} {
        switch $op {
            INC { set res [expr {[lindex $data {*}$args] + 1}] }
            DEC { set res [expr {[lindex $data {*}$args] - 1}] }
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
        # unpack ID
        lassign $id data q0
        set inp [expr {[lindex $data 0] != 0}]
        # get move
        lassign [lindex [my get $q0 $inp] 0] - - q1 ov
        lassign $ov op val
        switch $op {
            INC - DEC {
                lset data 0 [my ALU $op $data 0]
            }
            JZ - J {}
            PUSH {
                set data [linsert $data 0 $val]
            }
            POP {
                set data [lrange $data 1 end]
            }
            DUP {
                set data [linsert $data 0 [lindex $data 0]]
            }
            EQ - EQL - ADD - MUL {
                set v [my ALU $op $data 0 1]
                set data [lreplace $data 0 1 $v]
            }
            HALT {
                return
            }
            default {
                error \$op=$op
            }
        }
        # build new ID
        my addNewIDs [list $data $q1]
    }

    method ExecCounter id {
        # unpack ID
        lassign $id data q0 flag
        # get move
        lassign [lindex [my get $q0 $flag] 0] - - q1 or
        set rs [lassign $or op]
        lassign $rs r
        lassign $rs r0 r1 r2
        switch $op {
            INC - DEC {
                lset data $r0 [my ALU $op $data $r0]
            }
            JZ {}
            CLR {
                lset data $r0 0
            }
            CPY {
                lset data $r1 [lindex $data $r0]
            }
            EQ - EQL - ADD - MUL {
                set v [my ALU $op $data $r0 $r1]
                lset data $r2 $v
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
        set f [expr {[lindex $data $r] != 0}]
        my addNewIDs [list $data $q1 $f]
    }

}
