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
        log::log d "val = [my _get val $row] ($symbol)"
        return $symbol
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

    method succ {name val} {
        #: Given a value, find the next value in the component.
        set value [my get $name]
        set idx [lsearch $value $val]
        if {$idx < 0} {
            return -code error [format {can't find value}]
        }
        incr idx
        if {$idx >= [llength $value]} {
            return -code error [format {no successor to %s} $val]
        }
        return [lindex $value $idx]
    }


    method in {name symbol} {
        log::log d [info level 0] 
        expr {$symbol in [my getval [my getrow $name]]}
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

    method document {} {
        ;
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

    method get {key1 {key2 *}} {
        log::log d [info level 0] 
        log::log d "[my matrix serialize]"
        set rows1 [lmap idx [my matrix search rect 0 2 0 end $key1] {
            lindex $idx 1
        }]
        set rows $rows1
        if {$key2 ne "*"} {
            log::log d \$key2=$key2 
            set rows2 [lmap idx [my matrix search rect 1 2 1 end $key2] {
                lindex $idx 1
            }]
            log::log d \$rows2=$rows2 
            set rows [list]
            for {set row 2} {$row < [my matrix rows]} {incr row} {
                if {$row in $rows1 && $row in $rows2} {
                    lappend rows $row
                }
            }
        }
        set result [list]
        foreach row $rows {
            lappend result [my matrix get row $row]
        }
        return $result
    }

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
                    set symbol [$types set $t $symbol]
                    lappend val $symbol
                }
            } else {
                set arg [$types set $t $arg]
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

    method document {} {
        ;
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
    variable types

    constructor args {
        set types [set [uplevel 1 {namespace which -variable types}]]
        ::struct::matrix matrix
        matrix add columns 3
        foreach {name desc type} $args {
            matrix add row [list $name $desc $type]
        }
    }

    forward matrix matrix

    method make args {
        log::log d [info level 0] 
        log::log d [$types matrix serialize]
        set res [dict create]
        for {set row 0} {$row < [my matrix rows]} {incr row} {
            set val [lindex $args $row]
            # TODO check valid input
            set key [my matrix get cell 0 $row]
            dict set res $key {}
            foreach symbol $val {
                $types set [string index [my matrix get cell 2 $row] 0] $symbol
                log::log d "type = [my matrix get cell 2 $row]"
                if {[string index [my matrix get cell 2 $row] 1] eq "*"} {
                    dict lappend res $key $symbol
                    log::log d "appending: \$res=$res"
                } else {
                    dict set res $key $symbol
                    log::log d "replacing: \$res=$res"
                }
            }
        }
        log::log d \$res=$res 
        return $res
    }

    method print {} {
        append str "Instantaneous description\n"
        for {set row 0} {$row < [my matrix rows]} {incr row} {
            lassign [my matrix get row $row] name desc type
            append str [format "%-22s %s: %s\n" $desc $name $type]
        }
        return $str
    }

    method document {} {
        ;
    }

}

oo::class create ::automata::Configuration {
    mixin ::automata::Documentation

    variable types table iddef components

    #: Handles machine configurations, including instantaneous descriptions.

    constructor args {
        set types [::automata::Types new]
        next {*}$args
    }

    method print {} {
        #: Print the configuration.
        append str [$types print]
        append str [$table print]
        append str [$iddef print]
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

    method table args {
        set table [::automata::Table new {*}$args]
    }

    method id def {
        set iddef [::automata::ID new {*}$def]
    }

    method get {what args} {
        switch $what {
            doc {
                my GetDoc {*}$args
            }
            default {
            }
        }
    }

    method add {what args} {
        switch $what {
            doc {
                my AddDoc {*}$args
            }
            default {
            }
        }
    }

}
