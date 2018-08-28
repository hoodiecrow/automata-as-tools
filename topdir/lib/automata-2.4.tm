
namespace eval automata {}

oo::class create ::automata::FSM {
    variable tuple T
    constructor args {
        lassign $args tuple
        set tuple [dict merge {A {} B {} Q {} T {} S {} F {}} $tuple]
        set T [dict get $tuple T]
    }

    method TGet args {
        if {[llength $args] == 2} {
            lassign $args q a
            if {[dict exists $T $q $a]} {
                list [dict get $T $q $a]
            }
        } else {
            lassign $args q
            if {[dict exists $T $q]} {
                dict get $T $q
            }
        }
    }

    method NextStateTuple {state tokenA q1 args} {
        lassign $state tapeA - tapeB
        list [linsert $tapeA end {*}$tokenA] $q1 [concat $tapeB {*}$args]
    }

    method accept tape {
        # stateTuples: list of <tape, q0>
        set result [list]
        set stateTuples [list]
        foreach s [dict get $tuple S] {
            lappend stateTuples [list [list {*}$tape] $s]
        }
        while {[llength $stateTuples] > 0} {
            log::log d \$stateTuples=$stateTuples
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                log::log d \$stateTuple=$stateTuple 
                lassign $stateTuple tape q0
                if {[llength $tape] > 0} {
                    log::log d "q1 = [my TGet $q0 [lindex $tape 0]]"
                    foreach qs [my TGet $q0 [lindex $tape 0]] {
                        foreach q1 $qs {
                            log::log d \$q1=$q1 
                            lappend _stateTuples [list [lrange $tape 1 end] $q1]
                        }
                    }
                }
                foreach epsilonMove [my TGet $q0 {}] {
                    log::log d \$epsilonMove=$epsilonMove 
                    lappend _stateTuples [list $tape $epsilonMove]
                }
            }
            set result $stateTuples
            set stateTuples $_stateTuples
        }
        foreach t $result {
            lassign $t tape q
            if {$q in [dict get $tuple F]} {
                return 1
            }
        }
        return 0
    }

    method generate n {
        # stateTuples: list of <tapeA, q0, tapeB>
        set result [list]
        set stateTuples [list]
        foreach s [dict get $tuple S] {
            lappend stateTuples [list {} $s {}]
        }
        while {[llength $stateTuples] > 0} {
            log::log d \$stateTuples=$stateTuples
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                log::log d \$stateTuple=$stateTuple 
                set q0 [lindex $stateTuple 1]
                if {$n > 0} {
                    dict for {tokenA m} [my TGet $q0] {
                        lappend _stateTuples [my NextStateTuple $stateTuple $tokenA {*}$m]
                    }
                }
                if no {
                    foreach epsilonMove [my TGet $q0 {}] {
                        log::log d \$epsilonMove=$epsilonMove 
                        lappend _stateTuples [my NextStateTuple $stateTuple {} {*}$epsilonMove]
                    }
                }
            }
            log::log d \$n=$n 
            if {$n > 0} {
                incr n -1
            } else {
                set result $stateTuples
                log::log d \$result=$result 
                break
            }
            set stateTuples $_stateTuples
        }
        return $result
    }

    method MatchTapeB {varName tokens0 tapeB} {
        upvar 1 $varName _tapeB
        set _tapeB [list]
        set i 0
        foreach t0 $tokens0 {
            if {$t0 ne [lindex $tapeB $i]} {
                return 0
            }
            incr i
        }
        set _tapeB [lrange $tapeB $i end]
        return 1
    }

    method recognize {tapeA tapeB} {
        # stateTuples: list of <tapeA, q0, tapeB>
        set result [list]
        set stateTuples [list]
        foreach s [dict get $tuple S] {
            lappend stateTuples [list [list {*}$tapeA] $s [list {*}$tapeB]]
        }
        while {[llength $stateTuples] > 0} {
            log::log d \$stateTuples=$stateTuples
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                log::log d \$stateTuple=$stateTuple 
                lassign $stateTuple tapeA q0 tapeB
                if {[llength $tapeA] > 0} {
                    foreach m [my TGet $q0 [lindex $tapeA 0]] {
                        if {[my MatchTapeB _tapeB [lrange $m 1 end] $tapeB]} {
                            lappend _stateTuples [list [lrange $tapeA 1 end] [lindex $m 0] $_tapeB]
                        }
                    }
                }
                foreach epsilonMove [my TGet $q0 {}] {
                    log::log d \$epsilonMove=$epsilonMove 
                    lappend _stateTuples [my NextStateTuple $stateTuple {} {*}$epsilonMove]
                    if {[my MatchTapeB _tapeB [lrange $epsilonMove 1 end] $tapeB]} {
                        lappend _stateTuples [list $tapeA [lindex $epsilonMove 0] $_tapeB]
                    }
                }
            }
            set result $stateTuples
            set stateTuples $_stateTuples
        }
        foreach t $result {
            lassign $t tapeA q tapeB
            if {$q in [dict get $tuple F] && [llength $tapeB] == 0} {
                return 1
            }
        }
        return 0
    }

}
