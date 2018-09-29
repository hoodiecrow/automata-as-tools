package require fileutil

namespace eval automata {}

oo::class create ::automata::Documentation {
    variable components doc

    method AddDoc {what args} {
        switch $what {
            preamble - language {
                dict set doc $what [lindex $args 0]
            }
            option - argument {
                dict lappend doc $what $args
            }
            default {
                ;
            }
        }
    }

    method GetDoc what {
        set res {}
        switch $what {
            preamble {
                append res [dict get $doc $what]
            }
            language {
                append res "\n| Instruction | Arguments | Description |\n"
                append res "| :--- | :---: | :--- |\n"
                foreach {code arg desc} [dict get $doc $what] {
                    append res [format "| `%s` | %s | %s |\n" \
                        $code \
                        $arg \
                        $desc]
                }
            }
            option {
                foreach opt [dict get $doc $what] {
                    switch [llength $opt] {
                        1 { append res "* `[lindex $opt 0]`\n" }
                        2 { append res "* `[lindex $opt 0]`: [lindex $opt 1]\n" }
                        default {
                            ;
                        }
                    }
                }
            }
            argument {
                if {[dict exists $doc $what]} {
                    foreach arg [dict get $doc $what] {
                        switch [llength $arg] {
                            1 { append res "* `[lindex $arg 0]`\n" }
                            2 { append res "* `[lindex $arg 0]`: [lindex $arg 1]\n" }
                            default {
                                ;
                            }
                        }
                    }
                }
            }
            default {
                ;
            }
        }
        return $res
    }

    method installRunMethod items {
        set options [list]
        foreach {name code desc} $items {
            if {[string match -* $name]} {
                lappend options $name [format {my %s $_args} $code]
                my add doc option $name $desc
            } elseif {$name eq "default"} {
                lappend options $name [format {my %s $args} $code]
                my add doc option $name $desc
            } else {
                my add doc argument $name $desc
            }
        }
        if {[llength $options] > 0} {
            oo::objdefine [self] method run args [format {
                set _args [lassign $args arg]
                switch $arg {%s}
            } $options]
        } else {
            oo::objdefine [self] forward run my Run
        }
    }

    method installOperations {instructionSet ops} {
        set lang [list]
        set actions [list]
        foreach {op fn desc} $ops {
            lappend lang $op [join [lsort -unique [regexp -all -inline {\*\w\*} $desc]] ,] $desc
            if {$op in [list {*}$instructionSet NOP]} {
                lappend actions $op [format {
                    lassign $regs a b c
                    apply {{i j a b c} {%s}} $i $j $a $b $c
                } $fn]
            }
        }
        lappend actions default {error [info level 0]}
        my add doc language $lang
        oo::objdefine [self] method GenOp {i j op regs} [format {
            switch $op {%s}
        } $actions]
    }

    method doc fn {
        set docstr {}
        set c [info object class [self]]
        dict for {name conf} $components {
            switch $name {
                table {
                    set comp2 [dict get $conf as]
                }
                id {
                    set comp3 [lmap m [dict get $conf members] {lindex $m 0}]
                }
                default {
                    lappend comp1 $name
                }
            }
        }
        append docstr "\n## Definition\n"
        append docstr "\n`$c` (class)\n"
        append docstr \n[string trim [my get doc preamble]]\n
        append docstr [format "\nThe configuration for %s is (%s | %s | %s)\n" \
            [namespace tail $c] \
            [join $comp1 ", "] \
            [join [string map {* \\\\*} $comp2] ×] \
            [join $comp3 ", "]]
        append docstr \n {where the [[defining tuple|dt]] is:} \n\n
        foreach name $comp1 {
            dict with components $name {
                append docstr "* `$name` is the "
                if {!$scalar} {
                    append docstr "set of "
                }
                append docstr [string tolower $label] " "
                if {[regexp {sym (ni|in) {([^}]+)} $exclude -> op symbols]} {
                    if {$op eq "in"} {
                        append docstr "(excluding "
                    } else {
                        append docstr "(= \x7b"
                    }
                    append docstr [join [lmap sym $symbols {
                        if {$sym eq {}} {
                            continue
                        } else {
                            set sym
                        }
                    }] ", "]
                    if {$op eq "ni"} {
                        append docstr "\x7d"
                    }
                    append docstr )
                }
                if {$superset ne {}} {
                    if {$scalar} {
                        append docstr "(∈ $superset)"
                    } else {
                        append docstr "(⊆ $superset)"
                    }
                }
                if {$firstof ne {}} {
                    append docstr "(first element of $firstof)"
                }
                switch $domain {
                    B { append docstr "(= {0, 1})" }
                    N { append docstr "(⊂ ℕ : 0, 1, 2, ...)" }
                    Z { append docstr "(⊂ ℤ : ..., −2, −1, 0, 1, 2, ...)" }
                    R { append docstr "(⊂ ℝ : real numbers)" }
                    default {
                        ;
                    }
                }
            }
            append docstr \n
        }
        append docstr \n {and the [[Instantaneous Description|id]] (ID) is:} \n\n
        foreach m [dict get $components id members] {
            lassign $m name type label
            append docstr [format "* `%s` : %-2s = %s\n" \
                $name \
                [string map {* \\*} $type] \
                [string tolower $label]]
        }
        if {[dict exists $doc language]} {
            append docstr "\n## Language\n"
            if no {
                append docstr "\n| :--- | :---: | :--- |\n"
                foreach {code arg desc} [dict get $doc language] {
                    append docstr [format "| `%s` | %s | %s |\n" \
                    $code \
                    $arg \
                    $desc]
                }
            }
            append docstr [my get doc language]
        }
        append docstr "\n## Usage\n"
        append docstr "\n*machine* `run` *?option...?* *?arg...?*\n"
        if {[dict exists $doc option]} {
            append docstr "\n### Options\n"
            append docstr \n [my get doc option] \n
        }
        append docstr "\n### Arguments\n"
        append docstr \n [my get doc argument] \n
        if {$fn ne {}} {
            ::fileutil::writeFile -encoding utf-8 $fn $docstr
        } else {
            return $docstr
        }
    }

}

