namespace eval automata {}

oo::class create ::automata::Machine {
    variable types

    constructor args {
        next {*}$args
    }

    method search {id fn {steps {}}} {
        if {$steps ne {}} {
            if {$steps <= 0} {
                return [list $id]
            } else {
                incr steps -1
            }
        }
        set ids [my $fn $id]
        if {[llength $ids] eq 0} {
            return [list $id]
        }
        set ids [lsort -unique $ids]
        return [concat {*}[lmap id $ids {
            my search $id $fn $steps
        }]]
    }

    method Move {varName1 varName2 dir} {
        log::log d [info level 0] 
        # Shared between BTM and PTM
        upvar 1 $varName1 tape $varName2 head
        switch $dir {
            L {
                incr head
                if {$head >= [expr {[llength $tape] - 1}]} {
                    lappend tape [lindex [$types get A] 0]
                }
            }
            R {
                if {$head < 1} {
                    set tape [linsert $tape 0 [lindex [$types get A] 0]]
                } else {
                    incr head -1
                }
            }
            N {}
        }
        return
    }

}
