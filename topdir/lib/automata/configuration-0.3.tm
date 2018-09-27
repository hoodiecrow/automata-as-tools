
namespace eval automata {}

oo::class create ::automata::Configuration {
    variable components table id doc

    #: Handles machine configurations, including instantaneous descriptions.

    constructor args {
        next {*}$args
    }

    method print {} {
        #: Print the configuration (without the ID).
        set str {}
        dict for {k v} $components {
            switch $k {
                table {
                    append str "Transitions\n"
                    append str [format "%-5s %-5s %-5s %s\n" q0 inp q1 out]
                    foreach tuple [dict get $v value] {
                        set out [lassign $tuple q0 inp q1]
                        if {$inp eq {}} {
                            set inp ε
                        }
                        append str [format "%-5s %-5s %-5s %s\n" $q0 $inp $q1 $out]
                    }
                }
                id { ; }
                default {
                    if {[dict get $v hide]} {
                        continue
                    }
                    set vals [dict get $v value]
                    set _vals [lmap val $vals {if {$val eq {}} {lindex ε} {set val}}]
                    if {[dict get $v scalar]} {
                        append str [format "%-15s %s = %s\n" [dict get $v label] $k $_vals]
                    } else {
                        append str [format "%-15s %s = {%s}\n" [dict get $v label] $k [join $_vals ", "]]
                    }
                }
            }
        }
        puts -nonewline $str
    }

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
        oo::objdefine [self] method run args [format {
            set _args [lassign $args arg]
            switch $arg {%s}
        } $options]
    }

    method doc {} {
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
        append docstr "\n\n## Definition\n"
        append docstr "\n`$c` (class)\n"
        append docstr \n[string trim [join [my get doc preamble]]]\n
        append docstr [format "\nThe configuration for %s is (%s | %s | %s)\n" \
        [namespace tail $c] \
        [join $comp1 ", "] \
        [join $comp2 ×] \
        [join $comp3 ", "]]
        append docstr \n {where the [[defining tuple|dt]] is:} \n\n
        foreach name $comp1 {
            append docstr [format "* `%s` = %s%s\n" \
            $name \
            [string tolower [dict get $components $name label]] \
            [if {[dict get $components $name superset] ne {}} {
                format " (⊆ %s)" [dict get $components $name superset]
            }]]
        }
        append docstr \n {and the [[Instantaneous Description|id]] (ID) is:} \n\n
        foreach name $comp3 label [lmap m [dict get $components id members] {lindex $m 2}] {
            append docstr [format "* `%s` = %s\n" \
            $name \
            [string tolower $label]]
        }
        append docstr "\n## Usage\n"
        append docstr "\n*machine* `run` *?option...?* *?arg...?*\n"
        append docstr "\n### Options\n"
        append docstr \n [my get doc option] \n
        append docstr "\n### Arguments\n"
        append docstr \n [my get doc argument] \n
    }

    method graded {label name args} {
        dict set components $name label $label
        dict set components $name type graded
        dict set components $name epsilon {v {set v}}
        dict set components $name exclude {v {set v}}
        dict set components $name insert {v {}}
        dict set components $name sorted 0
        dict set components $name hide 0
        dict set components $name scalar 0
        dict set components $name superset {}
        dict set components $name domain {}
        dict set components $name value {}
        while {[string match -* [lindex $args 0]]} {
            switch [lindex $args 0] {
                -epsilon {
                    set args [lassign $args - e]
                    dict set components $name epsilon [list sym [format {if {$sym eq "%s"} list {set sym}} $e]]
                }
                -enum {
                    set args [lassign $args - e]
                    dict set components $name value $e
                    dict set components $name exclude [list sym [format {if {$sym ni {%s}} list {set sym}} $e]]
                }
                -exclude {
                    set args [lassign $args - e]
                    dict set components $name exclude [list sym [format {if {$sym in {%s}} list {set sym}} $e]]
                }
                -insert {
                    set args [lassign $args - i]
                    dict set components $name insert [list sym [format {my add %s $sym} $i] [self namespace]]
                }
                -sorted {
                    set args [lassign $args -]
                    dict set components $name sorted 1
                }
                -hide {
                    set args [lassign $args -]
                    dict set components $name hide 1
                }
                -scalar {
                    set args [lassign $args -]
                    dict set components $name scalar 1
                }
                -superset {
                    set args [lassign $args - s]
                    dict set components $name superset $s
                }
                -default {
                    set args [lassign $args - d]
                    dict set components $name value $d
                }
                -domain {
                    set args [lassign $args - d]
                    dict set components $name domain $d
                    if {$d eq "B"} {
                        dict set components $name value {0 1}
                    }
                    dict set components $name sorted 1
                }
                default {
                    return -code error [format {unknown option "%s"} [lindex $args 0]]
                }
            }
        }
    }

    method table args {
        if {[dict exists $components table]} {
            return -code error [format {table already defined}]
        }
        set name table
        dict set components $name label Transitions
        dict set components $name type table
        dict set components $name as {}
        dict set components $name value {}
        while {[string match -* [lindex $args 0]]} {
            switch [lindex $args 0] {
                -as {
                    set args [lassign $args - a]
                    dict set components $name as $a
                }
            }
        }
    }

    method id desc {
        if {[dict exists $components id]} {
            return -code error [format {id format already defined}]
        }
        set name id
        dict set components $name label ID
        dict set components $name type id
        dict set components $name members {}
        dict with components $name {
            foreach {tag type label} $desc {
                lappend members [list $tag $type $label]
            }
        }
    }

    method Arrange {varName sorted} {
        upvar 1 $varName value
        if {$sorted} {
            set value [lsort -unique -dict $value]
        } else {
            set u [dict create]
            foreach item $value {
                dict set u $item 1
            }
            set value [dict keys $u]
        }
        return
    }

    method in {what args} {
        switch $what {
            table {
                my InTable {*}$args
            }
            id {
                my InID {*}$args
            }
            default {
                my InGraded $what {*}$args
            }
        }
    }

    method InGraded {name val} {
        dict with components $name {
            expr {$val in $value}
        }
    }

    method get {what args} {
        switch $what {
            table {
                my GetTable {*}$args
            }
            id {
                my GetID {*}$args
            }
            doc {
                my GetDoc {*}$args
            }
            default {
                my GetGraded $what {*}$args
            }
        }
    }

    method GetGraded {name args} {
        dict get $components $name value
    }

    method GetTable {q s} {
        dict with components table {
            set v [lsearch -all -inline -index 0 $value $q]
        }
        return [lsearch -all -inline -index 1 $v $s]
    }

    method add {what args} {
        switch $what {
            table {
                my AddTable {*}$args
            }
            id {
                my AddID {*}$args
            }
            doc {
                my AddDoc {*}$args
            }
            default {
                my AddGraded $what {*}$args
            }
        }
    }

    method AddGraded {name args} {
        if {[llength $args] > 0} {
            if {[my FitsGraded $name syms {*}$args]} {
                my AddGradedValue $name $syms
            } else {
                return -code error [format {can't add "%s" to %s} $syms $name]
            }
        }
    }

    method AddGradedValue {name syms} {
        dict with components $name {
            if {$scalar} {
                set sym [lindex $syms end]
                if {$sym ne {}} {
                    apply $insert $sym
                    set value $sym
                }
            } else {
                foreach sym $syms {
                    if {$sym ne {}} {
                        apply $insert $sym
                        lappend value $sym
                    }
                }
                my Arrange value $sorted
            }
        }
    }


    method FitsGraded {name varName args} {
        upvar 1 $varName syms
        set syms [list]
        dict with components $name {
            foreach sym $args {
                if {[llength $sym] > 1} {
                    return -code error [format {non-atomic symbol "%s"} $sym]
                }
                switch $domain {
                    B {
                        if {$sym ni {0 1}} {
                            return 0
                        }
                    }
                    N {
                        if {![string is digit -strict $sym]} {
                            return 0
                        }
                    }
                    Z {
                        if {![string is entier -strict $sym]} {
                            return 0
                        }
                    }
                    R {
                        if {![string is double -strict $sym]} {
                            return 0
                        }
                    }
                }
                set sym [apply $exclude $sym]
                if {$sym eq {}} {
                    return 0
                }
                set sym [apply $epsilon $sym]
                lappend syms $sym
            }
        }
        return 1
    }

    method AddTable args {
        log::log i [info level 0] 
        set name table
        set tuple [list]
        dict with components $name {
            foreach arg $args fmt $as {
                if {$fmt eq {}} {
                    return -code error [format {too many symbols}]
                }
                if {[string index $fmt end] eq "*"} {
                    set c [string trimright $fmt *]
                    if {[my FitsGraded $c syms {*}$arg]} {
                        my AddGradedValue $c $syms
                        lappend tuple $syms
                    } else {
                        return -code error [format {can't add "%s" to table} $syms]
                    }
                } else {
                    if {[my FitsGraded $fmt sym $arg]} {
                        my AddGradedValue $fmt $sym
                        lappend tuple [lindex $sym 0]
                    } else {
                        return -code error [format {can't add "%s" to table} $sym]
                    }
                }
            }
            lappend value $tuple
        }
    }

    method AddID args {
        log::log i [info level 0] 
        set name id
        set result [dict create]
        dict with components $name {
            foreach arg $args member $members {
                if {$member eq {}} {
                    return -code error [format {too many symbols}]
                }
                lassign $member key fmt
                if {[string index $fmt end] eq "*"} {
                    if {[my FitsGraded [string trimright $fmt *] syms {*}$arg]} {
                        dict set result $key $syms
                    } else {
                        return
                    }
                } else {
                    if {[my FitsGraded $fmt sym $arg]} {
                        dict set result $key $sym
                    } else {
                        return
                    }
                }
            }
        }
        return $result
    }

    method succ {what args} {
        switch $what {
            table - id {
                return -code error [format {unknown command "succ %s"} $what]
            }
            default {
                my Succ $what {*}$args
            }
        }
    }

    method Succ {name val} {
        #: Given a value, find the next value in the component.
        dict with components $name {
            set idx [lsearch $value $val]
            if {$idx < 0} {
                return -code error [format {can't find value}]
            }
            incr idx
            if {$idx >= [llength $value]} {
                return -code error [format {no successor to %s} $val]
            }
        }
        return [lindex $value $idx]
    }


}
