package require struct::matrix

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::documentation

namespace eval automata {}

oo::class create ::automata::Types {
    constructor args {
        ::struct::matrix matrix
        matrix add columns 5
    }

    forward matrix matrix

    method addrow {name desc type vals args} {
        set args [dict merge {
            -sorted 1
            -epsilon {}
            -index -1
            -hidden 0
        } $args]
        my matrix add row [list $name $type $vals $args $desc]
    }

    method getrow name {
        set found [my matrix search column 0 $name]
        if {[llength $found] > 0} {
            return [lindex $found 0 1]
        } else {
            error not-found
        }
    }

    method _set {key row val} {
        set col [lsearch {name type val opts desc} $key]
        if {$col >= 0} {
            my matrix set cell $col $row $val
        } else {
            return -code error [format {key "%s" not found} $key]
        }
    }

    method _get {key row} {
        set col [lsearch {name type val opts desc} $key]
        if {$col >= 0} {
            my matrix get cell $col $row
        } else {
            return -code error [format {key "%s" not found} $key]
        }
    }

    method set {name symbol} {
        log::log d [info level 0] 
        set row [my getrow $name]
        array set options [my _get opts $row]
        if no {
            if {$symbol eq {}} {
                return -code error [format {empty symbol}]
            }
        }
        if {$symbol eq $options(-epsilon)} {
            set symbol {}
        }
        if {[my _get type $row] ne "@"} {
            set old [my _get val $row]
            if {$options(-plural)} {
                if {$options(-sorted)} {
                    my _set val $row [lsort -dictionary -unique [linsert $old end $symbol]]
                } else {
                    my _set val $row [dict set old $symbol 1]
                }
            } else {
                my _set val $row $symbol
            }
            if {[my _get type $row] ni {# @ N}} {
                my set [my _get type $row] $symbol
            }
        }
        log::log d "val = [my _get val $row]"
    }

    forward getname my _get name
    forward gettype my _get type
    forward getopts my _get opts
    forward getdesc my _get desc
     
    method getval row {
        array set options [my _get opts $row]
        if {$options(-index) >= 0} {
            set type [my _get type $row]
            switch $type {
                "#" {
                    lindex [my _get val $row] $options(-index)
                }
                "N" {
                    return 0
                }
                default {
                    lindex [my get $type] $options(-index)
                }
            }
        } else {
            set type [my _get type $row]
            if {$type in {@ N}} {
                my _get val $row
            } else {
                if {$options(-plural)} {
                    if {$options(-sorted)} {
                        my _get val $row
                    } else {
                        dict keys [my _get val $row]
                    }
                } else {
                    my _get val $row
                }
            }
        }
    }

    method get name {
        log::log d [info level 0] 
        my getval [my getrow $name]
    }


    method print {} {
        set str {}
        for {set row 0} {$row < [my matrix rows]} {incr row} {
            array set options [my getopts $row]
            if {$options(-hidden)} {
                continue
            }
            if {$options(-plural)} {
                set values [lmap val [my getval $row] {
                    if {$val eq {}} {
                        continue
                    } else {
                        set val
                    }
                }]
                append str [format "%-15s %s = {%s}\n" [my getdesc $row] [my getname $row] [join $values ", "]]
            } elseif {[my _get type $row] eq "@"} {
                set values [my getval $row]
                if {[llength $values] eq 1} {
                    append str [format "%-15s %s = %s\n" [my getdesc $row] [my getname $row] $values]
                } else {
                    append str [format "%-15s %s = {%s}\n" [my getdesc $row] [my getname $row] [join $values ", "]]
                }
            } else {
                append str [format "%-15s %s = %s\n" [my getdesc $row] [my getname $row] [my getval $row]]
            }
        }
        return $str
    }

    method dump {} {
        set res {}
        for {set row 0} {$row < [my matrix rows]} {incr row} {
            append res [my matrix get row $row] \n
        }
        return $res
    }

}

oo::class create ::automata::Table {
    variable types

    constructor args {
        set types [set [uplevel 1 {namespace which -variable types}]]
        ::struct::matrix matrix
        matrix add columns [llength $args]
        matrix add row [lmap arg $args {string index $arg 0}]
        matrix add row [lmap arg $args {string index $arg 1}]
    }

    forward matrix matrix

    method add args {
        log::log d [info level 0] 
        if {[llength $args] ne [my matrix columns]} {
            return -code error [format {can't add table row}]
        }
        set values [list]
        set col 0
        foreach arg $args {
            set val {}
            set t [my matrix get cell $col 0]
            if {[llength $arg] > 1} {
                if {[my matrix get cell $col 1] ne "*"} {
                    return -code error [format {can't add multiple symbols}]
                }
                foreach symbol $arg {
                    $types set $t $symbol
                    lappend val $symbol
                }
            } else {
                $types set $t $arg
                set val $arg
            }
            lappend values $val
            incr col
        }
        my matrix add row $values
    }

    method print {} {
        append str "Transitions\n"
        append str [format "%-5s %-5s %-5s %s\n" q0 inp q1 out]
        for {set row 2} {$row < [my matrix rows]} {incr row} {
            set out [lassign [my matrix get row $row] q0 inp q1]
            if {$inp eq {}} {
                set inp ε
            }
            append str [format "%-5s %-5s %-5s %s\n" $q0 $inp $q1 $out]
        }
        return $str
    }

    method dump {} {
        set res {}
        for {set row 0} {$row < [my matrix rows]} {incr row} {
            append res [my matrix get row $row] \n
        }
        return $res
    }

}

oo::class create ::automata::ID {
    constructor args {
        ::struct::matrix matrix
        matrix add columns 3
        foreach {name desc type} $args {
            matrix add row [list $name $desc $type]
        }
    }

    forward matrix matrix

    method print {} {
        append str "Instantaneous description\n"
        for {set row 0} {$row < [my matrix rows]} {incr row} {
            lassign [my matrix get row $row] name desc type
            append str [format "%-22s %s: %s\n" $desc $name $type]
        }
        return $str
    }

}

oo::class create ::automata::Configuration {
    mixin ::automata::Documentation

    variable types table iddef components
    #table id
    # doc

    #: Handles machine configurations, including instantaneous descriptions.

    constructor args {
        set types [::automata::Types new]
        next {*}$args
    }

    method print {} {
        #: Print the configuration (without the ID).
        set str {}
        append str [$types print]
        dict for {k v} $components {
            switch $k {
                table {
                    append str [$table print]
                    if no {
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
                }
                id {
                    append str [$iddef print]
                }
                default {
                    if no {
                        if {[dict get $v hide]} {
                            continue
                        }
                        if {[dict get $v firstof] ne {}} {
                            set vals [lrange [dict get $components [dict get $v firstof] value] 0 0]
                        } else {
                            set vals [dict get $v value]
                        }
                        set _vals [lmap val $vals {if {$val eq {}} {lindex ε} {set val}}]
                        if {[dict get $v scalar]} {
                            append str [format "%-15s %s = %s\n" [dict get $v label] $k $_vals]
                        } else {
                            append str [format "%-15s %s = {%s}\n" [dict get $v label] $k [join $_vals ", "]]
                        }
                    }
                }
            }
        }
        puts -nonewline $str
    }

    method type {name desc base args} {
        if {[string match {[#A-Z]*} $base]} {
            lassign [split [string trim $base] {}] type plural
            set vals {}
        } else {
            set vals [lassign $base type]
            set plural {}
        }
        $types addrow $name $desc $type $vals -plural [expr {$plural eq "+"}] {*}$args
    }

    method table1 args {
        set table [::automata::Table new {*}$args]
    }

    method id1 def {
        set iddef [::automata::ID new {*}$def]
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
        dict set components $name firstof {}
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
                -firstof {
                    set args [lassign $args - f]
                    dict set components $name firstof $f
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
        dict with components $name {
            if {$firstof eq {}} {
                return $value
            } else {
                return [lindex [my get $firstof] 0]
            }
        }
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
