package require struct::matrix

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::documentation

namespace eval automata {}

oo::class create ::automata::ValueSets {
    constructor args {
        ::struct::matrix matrix
        matrix add columns 5
    }

    forward Matrix matrix

    method ExtractRow found {
        if {[llength $found] > 0} {
            return [lindex $found 0 1]
        } else {
            error not-found
        }
    }

    method GetCol key {
        set col [lsearch {name vset val opts desc} $key]
        if {$col >= 0} {
            return $col
        } else {
            return -code error [format {key "%s" not found} $key]
        }
    }

    method addrow {name desc vset vals args} {
        set args [dict merge {
            -sorted 1
            -epsilon {}
            -index -1
            -hidden 0
        } $args]
        my Matrix add row [list $name $vset $vals $args $desc]
    }

    method Set {key row val} {
        my Matrix set cell [my GetCol $key] $row $val
    }

    method Get {key row} {
        my Matrix get cell [my GetCol $key] $row
    }

    method set {name symbol} {
        set row [my ExtractRow [my Matrix search column 0 $name]]
        array set options [my Get opts $row]
        if no {
            # TODO would be nice to have at initial assignment
            if {$symbol eq {}} {
                return -code error [format {empty symbol}]
            }
        }
        if {$symbol eq $options(-epsilon)} {
            set symbol {}
        }
        # first element eq @ means that the value is an immutable enumeration
        if {[my Get vset $row] ne "@"} {
            set old [my Get val $row]
            if {$options(-plural)} {
                if {$symbol ni $old} {
                    set new [linsert $old end $symbol]
                    if {$options(-sorted)} {
                        my Set val $row [lsort -dictionary -unique $new]
                    } else {
                        my Set val $row $new
                    }
                }
            } else {
                my Set val $row $symbol
            }
            # if the vset has an extendable subset, extend it
            set sup [my Get vset $row]
            if {$sup ni {# @ N}} {
                my set $sup $symbol
            }
        }
        return $symbol
    }

    forward getname my Get name
    forward gettype my Get vset
    forward getopts my Get opts
    forward getdesc my Get desc

    method getval row {
        array set options [my Get opts $row]
        if {$options(-index) >= 0} {
            set vset [my Get vset $row]
            switch $vset {
                "#" {
                    lindex [my Get val $row] $options(-index)
                }
                "N" {
                    return $options(-index)
                }
                default {
                    lindex [my get $vset] $options(-index)
                }
            }
        } else {
            my Get val $row
        }
    }

    method get name {
        my getval [my ExtractRow [my Matrix search column 0 $name]]
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
        expr {$symbol in [my getval [my ExtractRow [my Matrix search column 0 $name]]]}
    }

    method print {} {
        set str {}
        for {set row 0} {$row < [my Matrix rows]} {incr row} {
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
            } elseif {[my Get vset $row] eq "@"} {
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
        for {set row 0} {$row < [my Matrix rows]} {incr row} {
            append res [my Matrix get row $row] \n
        }
        return $res
    }

}

oo::class create ::automata::Table {
    constructor args {
        ::struct::matrix matrix
        matrix add rows 2
        foreach arg $args {
            matrix add column [split $arg {}]
        }
    }

    forward Matrix matrix

    method get {key1 {key2 *}} {
        set rows1 [lmap idx [my Matrix search rect 0 2 0 end $key1] {
            lindex $idx 1
        }]
        set rows $rows1
        if {$key2 ne "*"} {
            set rows2 [lmap idx [my Matrix search rect 1 2 1 end $key2] {
                lindex $idx 1
            }]
            set rows [list]
            for {set row 2} {$row < [my Matrix rows]} {incr row} {
                if {$row in $rows1 && $row in $rows2} {
                    lappend rows $row
                }
            }
        }
        lmap row $rows {
            my Matrix get row $row
        }
    }

    method add args {
        if {[llength $args] ne [my Matrix columns]} {
            return -code error [format {can't add table row}]
        }
        set values [list]
        set col 0
        foreach arg $args {
            set val {}
            set t [my Matrix get cell $col 0]
            if {[llength $arg] > 1} {
                if {[my Matrix get cell $col 1] ne "*"} {
                    return -code error [format {can't add multiple symbols}]
                }
                foreach symbol $arg {
                    set symbol [my vsets set $t $symbol]
                    lappend val $symbol
                }
            } else {
                set arg [my vsets set $t $arg]
                set val $arg
            }
            lappend values $val
            incr col
        }
        my Matrix add row $values
    }

    method print {} {
        append str "Transitions\n"
        append str [format "%-5s %-5s %-5s %s\n" q0 inp q1 out]
        for {set row 2} {$row < [my Matrix rows]} {incr row} {
            set out [lassign [my Matrix get row $row] q0 inp q1]
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
        for {set row 0} {$row < [my Matrix rows]} {incr row} {
            append res [my Matrix get row $row] \n
        }
        return $res
    }

}

oo::class create ::automata::ID {
    constructor args {
        ::struct::matrix matrix
        matrix add columns 3
        foreach {name desc vset} $args {
            matrix add row [list $name $desc $vset]
        }
    }

    forward Matrix matrix

    method make args {
        set res [dict create]
        for {set row 0} {$row < [my Matrix rows]} {incr row} {
            set val [lindex $args $row]
            # TODO check valid input
            set key [my Matrix get cell 0 $row]
            dict set res $key {}
            foreach symbol $val {
                my vsets set [string index [my Matrix get cell 2 $row] 0] $symbol
                if {[string index [my Matrix get cell 2 $row] 1] eq "*"} {
                    dict lappend res $key $symbol
                } else {
                    dict set res $key $symbol
                }
            }
        }
        return $res
    }

    method print {} {
        append str "Instantaneous description\n"
        for {set row 0} {$row < [my Matrix rows]} {incr row} {
            lassign [my Matrix get row $row] name desc vset
            append str [format "%-22s %s: %s\n" $desc $name $vset]
        }
        return $str
    }

    method document {} {
        ;
    }

}

oo::class create ::automata::Configuration {
    mixin ::automata::Documentation

    variable vsets table iddef components

    #: Handles machine configurations, including instantaneous descriptions.

    constructor args {
        set vsets [::automata::ValueSets new]
        oo::objdefine [self] forward vsets $vsets
        oo::define ::automata::Table forward vsets $vsets
        oo::define ::automata::ID forward vsets $vsets
        next {*}$args
    }

    destructor {
        catch {$vsets destroy}
    }

    method print {} {
        #: Print the configuration.
        append str [my vsets print]
        append str [$table print]
        append str [$iddef print]
        puts -nonewline $str
    }

    method values {name desc base args} {
        if {[string match {[#A-Z]*} $base]} {
            lassign [split [string trim $base] {}] vset plural
            set vals {}
        } else {
            set vals [lassign $base vset]
            set plural {}
        }
        my vsets addrow $name $desc $vset $vals -plural [expr {$plural eq "+"}] {*}$args
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