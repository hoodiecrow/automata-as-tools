namespace eval automata {}

oo::class create ::automata::Operator {
    #: In the Processor, data is in a sequential-accessed sequence that can
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
    #:
    #: The method Blank from the main class is used.

    variable ns

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
                    lappend t [my Blank]
                }
            }
            R {
                if {$h < 1} {
                    set t [linsert $t 0 [my Blank]]
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
        lassign $id t q0 h
        set tuples [my get $q0 [lindex $t $h]]
        # should always be 0 or 1 tuples
        return [lmap tuple $tuples {
            my Print t $h [lindex $tuple 3 0]
            my Move t h [lindex $tuple 3 1]
            # build new ID
            list $t [lindex $tuple 2] $h
        }]
    }

}
