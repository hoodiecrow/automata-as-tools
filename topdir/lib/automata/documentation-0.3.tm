package require fileutil

# TODO need a better value transfer method

namespace eval automata {

    variable operations {
        JE:    {Jump to address *a* on *b* = *c*}
        JNE:   {Jump to address *a* on *b* ≠ *c*}
        JZ:    {Jump to address *a* on *b* = 0}
        JNZ:   {Jump to address *a* on *b* ≠ 0}
        JSE:   {Jump to address *a* on [<i>ToS<sub>0</sub></i>] = [<i>ToS<sub>1</sub></i>]}
        JSNE:  {Jump to address *a* on [<i>ToS<sub>0</sub></i>] ≠ [<i>ToS<sub>1</sub></i>]}
        JSZ:   {Jump to address *a* on <i>ToS</i> = 0}
        JSNZ:  {Jump to address *a* on <i>ToS</i> ≠ 0}
        JT:    {Jump to address *a* on <i>test</i> = 0}
        JNT:   {Jump to address *a* on <i>test</i> ≠ 0}
        J:     {Jump to address *a*}
        CALL:  {Call to address *a*}
        RET    {Return to previous address}
        HALT   {Stop the program}
        NOP    {No operation}
        MOVE   {Moves the robot one space forward}
        TURN   {Changes robot's facing counter-clockwards}
        TEST:  {Test some condition of the robot's world according to *label*}
        TAKE   {Transfer a beeper from square to bag (does nothing)}
        DROP   {Transfer a beeper from bag to square (does nothing)}
        PRINT: {Print symbol # *a* on tape}
        PRINT  {Print symbol #1 on tape}
        ERASE  {Print symbol #0 on tape}
        ROLL:  {Roll tape to the left (*a* = L) or right (*a* = R)}
        CLR:   {Set *a* to 0}
        INC:   {Increment *a*}
        DEC:   {Decrement *a*}
        CPY:   {Set *a* to *b*}
        PUSH   {Increment ToS, <i>ToS</i> ← <i>value</i>}
        INC    {Increment <i>ToS</i>}
        DEC    {Decrement <i>ToS</i>}
        CLR    {Set <i>ToS</i> to 0}
        DUP    {Increment ToS, <i>ToS<sub>0</sub></i> ← <i>ToS<sub>1</sub></i>}
        EQ     {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] eq [<i>ToS<sub>1</sub></i>]}
        EQL    {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] == [<i>ToS<sub>1</sub></i>]}
        MUL    {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] * [<i>ToS<sub>1</sub></i>]}
        ADD    {<i>ToS<sub>0,1</sub></i> ← [<i>ToS<sub>0</sub></i>] + [<i>ToS<sub>1</sub></i>]}
    }
}

oo::class create ::automata::Documentation {
    variable components doc

    method AddDoc {what args} {
        switch $what {
            preamble {
                dict set doc $what [lindex $args 0]
            }
            option - argument - language {
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
                foreach operation [dict get $doc $what] {
                    append res [format "| `%s` | %s | %s |\n" {*}$operation]
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

    method installOperations instructionSet {
        foreach {op desc} $::automata::operations {
            set parms [join [lsort -unique [regexp -all -inline {\*\w\*} $desc]] ,]
            if {$op in [list {*}$instructionSet NOP]} {
                my add doc language $op $parms $desc
            }
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

