
oo::class create ::automata::Component {
    variable data scalar superset nonempty
    constructor args {
        set scalar 0
        set superset {}
        set nonempty 0
        while {[llength $args] > 0} {
            switch [lindex $args 0] {
                -scalar {
                    set args [lassign $args -]
                    set scalar 1
                }
                -in {
                    set args [lassign $args - superset]
                }
                -nonempty {
                    set args [lassign $args -]
                    set nonempty 1
                }
                default {
                    break
                }
            }
        }
        lassign $args data
    }

    method exists {varName cond coll} {
        upvar 1 $varName var
        foreach var $coll {
            if {[uplevel 1 [list expr $cond]]} {
                return 1
            }
        }
        return 0
    }

    method forall {varName cond coll} {
        upvar 1 $varName var
        foreach var $coll {
            if {![uplevel 1 [list expr $cond]]} {
                return 0
            }
        }
        return 1
    }

    method get {} {
        return $data
    }

    method set args {
        foreach value $args {
            if {$value eq {} && $nonempty} {
                continue
            }
            if {$superset ne {}} {
                $superset set $value
            }
            if {$scalar} {
                set data $value
            } else {
                lappend data $value
            }
        }
        set data [lsort -unique $data]
    }
}
