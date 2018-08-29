
namespace eval automata {}

oo::class create ::automata::FSM {
    variable tuple T
    constructor args {
        lassign $args tuple
        set tuple [dict merge {A {} B {} Q {} T {} S {} F {}} $tuple]
        set T [dict get $tuple T]
    }

    method Moves q {
        if {[dict exists $tuple T $q]} {
            dict get $tuple T $q
        }
    }

    method Move {q a} {
        if {[dict exists $tuple T $q $a]} {
            dict get $tuple T $q $a
        }
    }

    method EpsilonMove q {
        tailcall my Move $q {}
    }

    method MatchA {varName token tapeA} {
        upvar 1 $varName _tapeA
        if {$token eq {}} {
            set _tapeA $tapeA
            return 1
        } elseif {$token eq [lindex $tapeA 0]} {
            set _tapeA [lrange $tapeA 1 end]
            return 1
        } else {
            return 0
        }
    }

    method MatchB {varName tokens tapeB} {
        upvar 1 $varName _tapeB
        set _tapeB [list]
        if {[llength $tokens] == 1 && [lindex $tokens 0] eq {}} {
            set _tapeB $tapeB
            return 1
        } else {
            foreach token $tokens {
                if {$token ne [lindex $tapeB 0]} {
                    return 0
                }
                set tapeB [lrange $tapeB 1 end]
            }
            set _tapeB $tapeB
            return 1
        }
    }

    method accept tape {
        # stateTuples: list of <tape, q0>
        set results [list]
        set stateTuples [list]
        foreach s [dict get $tuple S] {
            lappend stateTuples [list [list {*}$tape] $s]
        }
        while {[llength $stateTuples] > 0} {
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                lassign $stateTuple tape q0
                set moves [my Moves $q0]
                foreach token [dict keys $moves] {
                    if {[my MatchA _tape $token $tape]} {
                        set targets [dict get $moves $token]
                        foreach q1 $targets {
                            lappend _stateTuples [list $_tape $q1]
                        }
                    }
                }
            }
            set results $stateTuples
            set stateTuples $_stateTuples
        }
        foreach result $results {
            lassign $result tape q
            if {[llength $tape] == 0 && $q in [dict get $tuple F]} {
                return 1
            }
        }
        return 0
    }

    method recognize {tapeA tapeB} {
        # stateTuples: list of <tapeA, q0, tapeB>
        set results [list]
        set stateTuples [list]
        foreach s [dict get $tuple S] {
            lappend stateTuples [list [list {*}$tapeA] $s [list {*}$tapeB]]
        }
        while {[llength $stateTuples] > 0} {
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                lassign $stateTuple tapeA q0 tapeB
                set moves [my Moves $q0]
                foreach tokenA [dict keys $moves] {
                    if {[my MatchA _tapeA $tokenA $tapeA]} {
                        set targets [dict get $moves $tokenA]
                        foreach target $targets {
                            set tokens [lassign $target q1]
                            if {[my MatchB _tapeB $tokens $tapeB]} {
                                lappend _stateTuples [list $_tapeA $q1 $_tapeB]
                            }
                        }
                    }
                }
            }
            set results $stateTuples
            set stateTuples $_stateTuples
        }
        foreach result $results {
            lassign $result tapeA q tapeB
            if {
                [llength $tapeA] == 0 &&
                $q in [dict get $tuple F] &&
                [llength $tapeB] == 0
            } then {
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
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                lassign $stateTuple tapeA q0 tapeB
                if {$n > 0} {
                    dict for {tokenA targets} [my Moves $q0] {
                        foreach target $targets {
                            set tokens [lassign $target q1]
                            if {$tokenA eq {}} {
                                set t [list $tapeA $q1]
                            } else {
                                set t [list [linsert $tapeA end $tokenA] $q1]
                            }
                            if {[llength $tokens] == 1 && [lindex $tokens 0] eq {}} {
                                lappend t $tapeB
                            } else {
                                lappend t [concat $tapeB $tokens]
                            }
                            lappend _stateTuples $t
                        }
                    }
                }
            }
            if {$n > 0} {
                incr n -1
            } else {
                set result $stateTuples
                break
            }
            set stateTuples $_stateTuples
        }
        return $result
    }

    method translate tapeA {
        # stateTuples: list of <tapeA, q0, tapeB>
        set results [list]
        set stateTuples [list]
        foreach s [dict get $tuple S] {
            lappend stateTuples [list [list {*}$tapeA] $s {}]
        }
        while {[llength $stateTuples] > 0} {
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                lassign $stateTuple tapeA q0 tapeB
                set moves [my Moves $q0]
                foreach tokenA [dict keys $moves] {
                    if {[my MatchA _tapeA $tokenA $tapeA]} {
                        set targets [dict get $moves $tokenA]
                        foreach target $targets {
                            set tokens [lassign $target q1]
                            if {[llength $tokens] == 1 && [lindex $tokens 0] eq {}} {
                                lappend _stateTuples [list $_tapeA $q1 $tapeB]
                            } else {
                                lappend _stateTuples [list $_tapeA $q1 [concat $tapeB $tokens]]
                            }
                        }
                    }
                }
            }
            set results $stateTuples
            set stateTuples $_stateTuples
        }
        return [lmap result $results {lindex $result 2}]
    }

    method reconstruct tapeB {
        # stateTuples: list of <tapeA, q0, tapeB>
        set results [list]
        set stateTuples [list]
        foreach s [dict get $tuple S] {
            lappend stateTuples [list {} $s [list {*}$tapeB]]
        }
        while {[llength $stateTuples] > 0} {
            set _stateTuples [list]
            foreach stateTuple $stateTuples {
                lassign $stateTuple tapeA q0 tapeB
                set moves [my Moves $q0]
                foreach tokenA [dict keys $moves] {
                    set targets [dict get $moves $tokenA]
                    foreach target $targets {
                        set tokens [lassign $target q1]
                        if {[my MatchB _tapeB $tokens $tapeB]} {
                            if {$tokenA eq {}} {
                                lappend _stateTuples [list $tapeA $q1 $_tapeB]
                            } else {
                                lappend _stateTuples [list [linsert $tapeA 0 $tokenA] $q1 $_tapeB]
                            }
                        }
                    }
                }
            }
            set results $stateTuples
            set stateTuples $_stateTuples
        }
        return [lmap result $results {lindex $result 0}]
    }

}
