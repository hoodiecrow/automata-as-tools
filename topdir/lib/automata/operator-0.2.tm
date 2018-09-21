namespace eval automata {}

oo::class create ::automata::Operator {
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
            return -code error [format {non-determinism detected: {%s}} $tuples]
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

}
