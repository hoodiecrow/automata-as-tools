package require fileutil

# TODO need a better value transfer method

namespace eval automata {

    variable operations {
        JE:    {je   $a $b $c}     {Jump to address *a* on *b* = *c*}
        JNE:   {jne  $a $b $c}     {Jump to address *a* on *b* ≠ *c*}
        JZ:    {jz   $a $b}        {Jump to address *a* on *b* = 0}
        JNZ:   {jnz  $a $b}        {Jump to address *a* on *b* ≠ 0}
        JT:    {jt   $a}           {Jump to address *a* on <i>test</i> = 0}
        JNT:   {jnt  $a}           {Jump to address *a* on <i>test</i> ≠ 0}
        J:     {j    $a}           {Jump to address *a*}
        CALL:  {call $a $m}        {Call to address *a*, sets flag}
        RET    ret                 {Return to previous address, sets flag}
        HALT   halt                {Stop the program}
        NOP    nop                 {No operation}
        MOVE   move                {Moves the robot one space forward}
        TURN   turn                {Changes robot's facing counter-clockwards}
        TEST:  test               {Test by parameters *d* and *e*}
        TAKE   take                {Transfer a beeper from square to bag (does nothing)}
        DROP   drop                {Transfer a beeper from bag to square (does nothing)}
        PRINT: {print $a}          {Print symbol # *a* on tape}
        PRINT  print               {Print symbol # *a* on tape}
        ROLL   {roll $a}           {Roll tape to the left (L) or right (R)}
        CLR:   {set $a 0}          {Set *a* to 0}
        store  {store $a $b}       {Store a number}
        INC:   {inc $a}            {Increment *a*}
        DEC:   {dec $a}            {Decrement *a*}
        CPY:   {set $a $b}         {Set *a* to *b*}
        upl    {store $a $a + 0}   {Unary plus}
        umn    {store $a 0 - $a}   {Unary minus}
        bnot   {store $a ~ $a}     {Bit. not}
        lnot   {store $a ! $a}     {Log. not}
        mul    {store $a $b * $c}  {Multiplication}
        div    {store $a $b / $c}  {Division}
        mod    {store $a $b % $c}  {Modulo}
        add    {store $a $b + $c}  {Addition}
        sub    {store $a $b - $c}  {Subtraction}
        lt     {store $a $b < $c}  {Less than}
        le     {store $a $b <= $c} {Less than or equal to}
        gt     {store $a $b > $c}  {Greater than}
        ge     {store $a $b >= $c} {Greater than or equal to}
        eq     {store $a $b eq $c} {String equality}
        eql    {store $a $b == $c} {Num. equality}
        neq    {store $a $b != $c} {Num. inequality}
        band   {store $a $b & $c}  {Bit. and}
        bxor   {store $a $b ^ $c}  {Bit. xor}
        bor    {store $a $b | $c}  {Bit. or}
        land   {store $a $b && $c} {Log. and}
        lor    {store $a $b || $c} {Log. or}
    }
}

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

    method installOperations instructionSet {
        set lang [list]
        set actions [list]
        foreach {op o desc} $::automata::operations {
            lappend lang $op [join [lsort -unique [regexp -all -inline {\*\w\*} $desc]] ,] $desc
            if {$op eq "<number>"} {
                set op PUSH
            }
            if {$op in [list {*}$instructionSet NOP]} {
                lappend actions $op [format {lindex {%s}} $o]
            }
        }
        if {![dict exists $actions PUSH]} {
            lappend actions PUSH {}
        }
        lappend actions default {error [info level 0]}
        log::log d \$actions=$actions 
        my add doc language $lang
        oo::objdefine [self] method GenOp op [format {
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

