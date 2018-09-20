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
        log::log d [info level 0] 
        switch $op {
            INC { expr {[lindex $args 0] + 1} }
            DEC { expr {[lindex $args 0] - 1} }
            default {
                ::tcl::mathop::$op {*}$args
            }
        }
    }

    method ExecStack id {
        # unpack ID
        lassign $id stack q0
        # get move
        lassign [lindex [my get $q0 *] 0] - op addr val
        switch $op {
            INC {
                lset stack 0 [my ALU INC [lindex $stack 0]]
                set q1 [my Q succ $q0]
            }
            DEC {
                lset stack 0 [my ALU DEC [lindex $stack 0]]
                set q1 [my Q succ $q0]
            }
            JZ {
                set v [my ALU == [lindex $stack 0] 0]
                set q1 [if {$v} {set addr} {my Q succ $q0}]
            }
            JE {
                set v [my ALU == {*}[lrange $stack 0 1]]
                set q1 [if {$v} {set addr} {my Q succ $q0}]
            }
            J { set q1 $addr }
            CLR {
                set stack {}
                set q1 [my Q succ $q0]
            }
            DUP {
                set stack [linsert $stack 0 [lindex $stack 0]]
                set q1 [my Q succ $q0]
            }
            DUP2 {
                set stack [linsert $stack 0 [lrange $stack 0 1]]
                set q1 [my Q succ $q0]
            }
            PUSH {
                set stack [linsert $stack 0 $val]
                set q1 [my Q succ $q0]
            }
            ADD {
                set v [my ALU + {*}[lrange $stack 0 1]]
                set stack [lreplace $stack 0 1 $v]
                set q1 [my Q succ $q0]
            }
            MUL {
                set v [my ALU * {*}[lrange $stack 0 1]]
                set stack [lreplace $stack 0 1 $v]
                set q1 [my Q succ $q0]
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
        lassign $id regs q0
        # get move
        lassign [lindex [my get $q0 *] 0] - op addr reg
        switch $op {
            INC {
                lset regs $reg [my ALU INC [lindex $regs $reg]]
                set q1 [my Q succ $q0]
            }
            DEC {
                lset regs $reg [my ALU DEC [lindex $regs $reg]]
                set q1 [my Q succ $q0]
            }
            JZ {
                set v [my ALU == [lindex $regs $reg] 0]
                set q1 [if {$v} {set addr} {my Q succ $q0}]
            }
            JE {
                lassign [split $reg ,] r0 r1
                set v [my ALU == [lindex $regs $r0] [lindex $regs $r1]]
                set q1 [if {$v} {set addr} {my Q succ $q0}]
            }
            CLR {
                lset regs $reg 0
                set q1 [my Q succ $q0]
            }
            CPY {
                lassign [split $reg ,] r0 r1
                lset regs $r1 [lindex $regs $r0]
                set q1 [my Q succ $q0]
            }
            ADD {
                lassign [split $reg ,] r0 r1 r2
                set v [my ALU + [lindex $regs $r0] [lindex $regs $r1]]
                lset regs $r2 $v
                set q1 [my Q succ $q0]
            }
            MUL {
                lassign [split $reg ,] r0 r1 r2
                set v [my ALU * [lindex $regs $r0] [lindex $regs $r1]]
                lset regs $r2 $v
                set q1 [my Q succ $q0]
            }
            HALT {
                return
            }
            default {
                error \$op=$op
            }
        }
        # build new ID
        my addNewIDs [list $regs $q1]
    }

}
