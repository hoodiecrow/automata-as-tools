namespace eval automata {}

oo::class create ::automata::Processor {
    #: In the Processor, data is in a direct-accessed sequence.
    #: For the Stack machine, accesses are relative to the stack top (#0).
    #: For the Counter machine, accesses are by absolute index.

    method ALU {op data args} {
        log::log d [info level 0] 
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
        # unpack ID
        lassign $id data q0 flag
        set _tail [lassign $data TOP]
        # get move
        lassign [lindex [my get $q0 $flag] 0] - - q1 op val r0 r1
        switch $op {
            PUSH {
                set data [linsert $data 0 $val]
            }
            INC - DEC - CLR {
                lset data 0 [my ALU $op $data 0]
            }
            DUP {
                set data [linsert $data 0 $TOP]
            }
            eq - == - + - * {
                set v [my ALU $op $data 0 1]
                set data [lreplace $data 0 1 $v]
            }
            {} {}
            default {
                error \$op=$op,\ \$val=$val
            }
        }
        log::log d \$data=$data 
        if {[my F contains $q1]} {
            return
        }
        # build new ID
        set flag [expr {[lindex $data 0] != 0}]
        return [list [list $data $q1 $flag]]
    }

    method ExecCounter id {
        log::log d [info level 0] 
        # unpack ID
        lassign $id data q0 flag
        # get move
        lassign [lindex [my get $q0 $flag] 0] - - q1 op r0 r1 r2
        # instruction set, after Shepherdson and Sturgis (1963)
        switch $op {
            INC { lset data $r0 [expr {[lindex $data $r0] + 1}] }
            DEC { lset data $r0 [expr {[lindex $data $r0] - 1}] }
            CLR { lset data $r0 0 }
            CPY { lset data $r1 [lindex $data $r0] }
        }
        if {[my F contains $q1]} {
            return
        }
        # build new ID
        set r [lindex [my get $q1 *] 0 4]
        set f [expr {[lindex $data $r] != 0}]
        return [list [list $data $q1 $f]]
    }

}
