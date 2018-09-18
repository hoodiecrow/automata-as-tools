namespace eval automata {}

oo::class create ::automata::Processor {

    method process id {
        lassign $id t q0 h
        set tuples [my get $q0 [lindex $t $h]]
        if {[llength $tuples] == 0} {
            return
        } elseif {[llength $tuples] > 1} {
            return -code error [format {non-determinism detected: (%s)} $tuples]
        }
        lassign $tuples tuple
        lassign $tuple - inp q1 out
        lassign $out osym move
        if {$osym ne "N"} {
            lset t $h $osym
        }
        switch $move {
            R { my tapeMoveR t h }
            L { my tapeMoveL t h }
            N {}
            default {
                error \$move=$move
            }
        }
        my addNewIDs [list $t $q1 $h]
    }

}
