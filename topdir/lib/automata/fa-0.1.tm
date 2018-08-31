
namespace eval automata {}

oo::class create ::automata::fa {
    # implements a similar interface to Tcllib ::grammar::fa
    variable tuple has

    method serialize {{type grammar::fa}} {
        # TODO loses output information
        # change type to automata::fa and add an output item
        set res [list $type]
        lappend res [lsort -unique [concat [dict get $tuple A] [dict get $tuple B]]]
        dict with tuple {
            foreach s $Q {
                set ss [expr {$S eq {} || $s in $S}]
                set fs [expr {$F eq {} || $s in $F}]
                set outputs {}
                dict set d $s [list $ss $fs [concat {*}[lmap {sym targets} [dict get $T $s] {
                    concat {*}[lmap target $targets {
                        set tokens [lassign $target q]
                        dict set outputs $sym $tokens
                        list $sym $q
                    }]
                }]]]
                dict set o $s $outputs
            }
        }
        switch $type {
            grammar::fa { lappend res $d }
            automata::fa { lappend res $d $o }
            default {
                error {unexpected alternative}
            }
        }
    }

    method deserialize serialization {
        lassign $serialization type symbols transitions outputs
        set m [::automata::FSM new]
        switch $type {
            grammar::fa {
                dict for {s transition} $transitions {
                    lassign $transition ss fs d
                    dict for {sym q} $d {
                        $m next $s $sym --> $q
                    }
                    if {$ss} {
                        $m start add $s
                    }
                    if {$fs} {
                        $m final add $s
                    }
                }
            }
            automata::fa {
                dict for {s transition} $transitions {
                    lassign $transition ss fs d
                    dict for {sym q} $d {
                        $m next $s $sym --> [list $q {*}[dict get $outputs $s $sym]]
                    }
                    if {$ss} {
                        $m start add $s
                    }
                    if {$fs} {
                        $m final add $s
                    }
                }
            }
            default {
                error {unexpected alternative}
            }
        }
    }

    method states {} {
        dict get $tuple Q
    }

    method state {verb args} {
        switch $verb {
            add { error {not implemented yet} }
            delete { error {not implemented yet} }
            exists { expr {[lindex $args 0] in [dict get $tuple Q]} }
            rename {
                lassign $args s snew
                if {$s ni [dict get $tuple Q]} {
                    return -code error [format {state "%s" not in state set} $s]
                    if {$s in [dict get $tuple Q]} {
                        return -code error [format {state "%s" already in state set} $snew]
                    }
                }
                error {not implemented yet}
            }
            default {
                error {unexpected alternative}
            }
        }
    }

    method startstates {} {
        dict get $tuple S
    }

    method start? s {
        expr {$s in [dict get $tuple S]}
    }

    method start?set stateset {
        dict with tuple {
            foreach s $stateset {
                if {$s in $S} {
                    return 1
                }
            }
        }
        return 0
    }

    method start {verb args} {
        dict with tuple {
            switch $verb {
                add {
                    lassign $args s
                    if {$s ni [dict get $tuple Q]} {
                        return -code error [format {state "%s" not in state set} $s]
                    }
                    if {$s ni [dict get $tuple S]} {
                        lappend S $s
                    }
                }
                remove { error {not implemented yet} }
                default {
                    error {unexpected alternative}
                }
            }
        }
    }

    method finalstates {} {
        dict get $tuple F
    }

    method final? s {
        expr {$s in [dict get $tuple F]}
    }

    method final?set stateset {
        dict with tuple {
            foreach s $stateset {
                if {$s in $F} {
                    return 1
                }
            }
        }
        return 0
    }

    method final {verb args} {
        dict with tuple {
            switch $verb {
                add {
                    lassign $args s
                    if {$s ni [dict get $tuple Q]} {
                        return -code error [format {state "%s" not in state set} $s]
                    }
                    if {$s ni [dict get $tuple F]} {
                        lappend F $s
                    }
                }
                remove { error {not implemented yet} }
                default {
                    error {unexpected alternative}
                }
            }
        }
    }

    method symbols {} {
        concat [dict get $tuple A] [dict get $tuple B]
    }

    method symbols@ args {
        switch [llength $args] {
            1 { my SymbolsS {*}$args }
            2 { my SymbolsT {*}$args }
            default {
                error {unexpected alternative}
            }
        }
    }

    method SymbolsS s {
        set result [list]
        if {[dict exists $tuple T $s]} {
            dict for {token -} [dict get $tuple T $s] {
                lappend result $token
            }
        }
        return $result
    }

    method SymbolsT {s t} {
        set result [list]
        if {[dict exists $tuple T $s]} {
            dict for {token targets} [dict get $tuple T $s] {
                foreach target $targets {
                    if {[lindex $target 0] eq $t} {
                        lappend result $token
                    }
                }
            }
        }
        return $result
    }

    method symbols@set stateset {
        set result [list]
        foreach s $stateset {
            lappend result {*}[my SymbolsS $s]
        }
        return [lsort -unique $result]
    }

    method symbol {verb args} {
        switch $verb {
            add {
                dict with tuple {
                    set A [lsort -unique [list {*}$A [lindex $args 0]]]
                }
            }
            delete { error {not implemented yet} }
            rename { error {not implemented yet} }
            exists {
                dict with tuple {
                    expr {[lindex $args 0] in $A}
                }
            }
            default {
                error {unexpected alternative}
            }
        }
    }

    method next {s sym args} {
        if {[llength $args] != 2} {
            dict get $tuple T $s $sym
        } else {
            lassign $args - target
            if {![dict exists $tuple T $s]} {
                dict set tuple T $s {}
            }
            if {![dict exists $tuple T $s $sym]} {
                dict set tuple T $s $sym {}
            }
            dict with tuple T $s {
                lappend $sym [lindex $args 1]
            }
            my CompleteTuple
        }
    }

    method !next {s sym args} {
        error {not implemented yet}
    }

    method nextset {stateset sym} {
        set result [list]
        foreach s $stateset {
            lappend result {*}[my next $s $sym]
        }
        return [lsort -unique $result]
    }

    method is quality {
        switch $quality {
            deterministic {
                error {not implemented yet}
            }
            complete {
                error {not implemented yet}
            }
            useful {
                error {not implemented yet}
            }
            epsilon-free {
                expr {!$has(epsilon)}
            }
            default {
                error {unexpected alternative}
            }
        }
    }

    method reachable_states {} {
        error {not implemented yet}
    }
    method unreachable_states {} {
        error {not implemented yet}
    }
    method reachable s {
        error {not implemented yet}
    }
    method useful_states {} {
        error {not implemented yet}
    }
    method unuseful_states {} {
        error {not implemented yet}
    }
    method useful s {
        error {not implemented yet}
    }
    method epsilon_closure s {
        error {not implemented yet}
    }

}
