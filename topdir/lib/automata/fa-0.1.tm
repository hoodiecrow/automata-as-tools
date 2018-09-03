
namespace eval automata {}

oo::class create ::automata::fa {
    # implements a similar interface to Tcllib ::grammar::fa
    variable is

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

    forward edges my get T

    forward states my get Q
    forward state? my IsIn? Q

    method state {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my StateAdd $s }
            delete { error {not implemented yet} }
            exists { my state? $s }
            rename { error {not implemented yet} }
            default {
                error {unexpected alternative}
            }
        }
    }

    forward startstates my get S
    forward start? my IsIn? S
    forward start?set my IsInMultiple? S

    method start {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my StartAdd $s }
            remove { error {not implemented yet} }
            default {
                error {unexpected alternative}
            }
        }
    }

    forward finalstates my get F
    forward final? my IsIn? F
    forward final?set my IsInMultiple? F

    method final {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my FinalAdd $s }
            remove { error {not implemented yet} }
            default {
                error {unexpected alternative}
            }
        }
    }

    # symbols method delegated to ::automata::FSM

    method symbols@ args {
        switch [llength $args] {
            1 { my SymbolsFrom {*}$args }
            2 { my SymbolsFromTo {*}$args }
            default {
                error {unexpected alternative}
            }
        }
    }

    method symbols@set stateset {
        set result [list]
        foreach s $stateset {
            lappend result {*}[my SymbolsFrom $s]
        }
        return [lsort -unique $result]
    }

    method symbol {verb args} {
        set args [lassign $args s]
        switch $verb {
            add { my SymbolAdd $s }
            delete { error {not implemented yet} }
            rename { error {not implemented yet} }
            exists { expr {$s in [my symbols]} }
            default {
                error {unexpected alternative}
            }
        }
    }

    method next {s sym args} {
        if {$sym eq "ε"} {
            set sym {}
        }
        if {[llength $args] == 0} {
            my GetTarget $s $sym
        } else {
            my SetTarget $s $sym {*}[lmap token [lrange $args 1 end] {
                if {$token eq "ε"} {
                    set token {}
                } else {
                    set token
                }
            }]
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
        return $is($quality)
    }

    method reachable_states {} { error {not implemented yet} }
    method unreachable_states {} { error {not implemented yet} }
    method reachable s { error {not implemented yet} }
    method useful_states {} { error {not implemented yet} }
    method unuseful_states {} { error {not implemented yet} }
    method useful s { error {not implemented yet} }
    method epsilon_closure s { error {not implemented yet} }

}
