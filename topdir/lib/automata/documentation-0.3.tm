package require fileutil

namespace eval automata {}

oo::class create ::automata::Documentation {
    variable components doc

    method AddDoc {what args} {
        switch $what {
            preamble {
                dict set doc $what $args
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
        switch $what {
            preamble {
                dict get $doc $what
            }
            option {
                set res {}
                foreach opt [dict get $doc $what] {
                    switch [llength $opt] {
                        1 { append res "* `[lindex $opt 0]`\n" }
                        2 { append res "* `[lindex $opt 0]`: [lindex $opt 1]\n" }
                        default {
                            ;
                        }
                    }
                }
                return $res
            }
            argument {
                set res {}
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
                return $res
            }
            default {
                ;
            }
        }
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
        if no {
            error [format {
                set _args [lassign $args arg]
                switch $arg {%s}
            } $options]
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
        append docstr \n[string trim [join [my get doc preamble]]]\n
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
                if {[regexp {sym in {([^}]+)} $exclude -> symbols]} {
                    append docstr "(excluding "
                    append docstr [join [lmap sym $symbols {
                        if {$sym eq {}} {
                            continue
                        } else {
                            set sym
                        }
                    }] ", "]
                    append docstr )
                }
                if {[regexp {sym ni {([^}]+)} $exclude -> symbols]} {
                    append docstr (
                    append docstr [join [lmap sym $symbols {
                        if {$sym eq {}} {
                            continue
                        } else {
                            set sym
                        }
                    }] ", "]
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
        append docstr "\n## Usage\n"
        append docstr "\n*machine* `run` *?option...?* *?arg...?*\n"
        if {[dict exists doc option]} {
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

