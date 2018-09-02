
namespace eval automata {}

oo::class create ::automata::fa {
    # implements a similar interface to Tcllib ::grammar::fa
    variable has

    method serialize {{type grammar::fa}} {
        # TODO loses output information
        # change type to automata::fa and add an output item
        set res [list $type]
        lappend res [my symbols]
        foreach state [my states] {
            set outputs {}
            dict set d $state [list [my start? $state] [my final? $state] [concat {*}[lmap {sym targets} [my edges $state] {
                concat {*}[lmap target $targets {
                    set tokens [lassign $target q]
                    dict set outputs $sym $tokens
                    list $sym $q
                }]
            }]]]
            dict set o $state $outputs
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

    # states method delegated to ::automata::FSM
    # state? (added) method delegated to ::automata::FSM

    method state {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my AddState $s }
            delete { error {not implemented yet} }
            exists { my state? $s }
            rename { error {not implemented yet} }
            default {
                error {unexpected alternative}
            }
        }
    }

    # startstates method delegated to ::automata::FSM
    # start? method delegated to ::automata::FSM

    method start?set stateset {
        foreach s $stateset {
            if {[my start? $s]} {
                return 1
            }
        }
        return 0
    }

    method start {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my AddStart $s }
            remove { error {not implemented yet} }
            default {
                error {unexpected alternative}
            }
        }
    }

    # finalstates method delegated to ::automata::FSM
    # final? method delegated to ::automata::FSM

    method final?set stateset {
        foreach s $stateset {
            if {[my final? $s]} {
                return 1
            }
        }
        return 0
    }

    method final {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my AddFinal $s }
            remove { error {not implemented yet} }
            default {
                error {unexpected alternative}
            }
        }
    }

    # symbols method delegated to ::automata::FSM

    method symbols@ args {
        switch [llength $args] {
            1 { my SymbolsS {*}$args }
            2 { my SymbolsT {*}$args }
            default {
                error {unexpected alternative}
            }
        }
    }

    method symbols@set stateset {
        set result [list]
        foreach s $stateset {
            lappend result {*}[my SymbolsS $s]
        }
        return [lsort -unique $result]
    }

    method symbol {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my AddSymbol $s }
            delete { error {not implemented yet} }
            rename { error {not implemented yet} }
            exists { expr {$s in [my symbols]} }
            default {
                error {unexpected alternative}
            }
        }
    }

    method next {s sym args} {
        if {[llength $args] != 2} {
            my GetTarget $s $sym
        } else {
            my SetTarget $s $sym {*}$args
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
