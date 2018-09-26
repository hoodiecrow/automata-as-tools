
namespace eval automata {}

oo::class create ::automata::Configuration {
    variable components table id

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

    method graded {label name args} {
        dict set components $name label $label
        dict set components $name type graded
        dict set components $name epsilon {v {set v}}
        dict set components $name exclude {v {set v}}
        dict set components $name insert {v {}}
        dict set components $name scalar 0
        dict set components $name domain {}
        dict set components $name value {}
        while {[string match -* [lindex $args 0]]} {
            switch [lindex $args 0] {
                -epsilon {
                    set args [lassign $args - e]
                    dict set components $name epsilon [list [list sym] [format {if {$sym eq "%s"} list {set sym}} $e]]
                }
                -exclude {
                    set args [lassign $args - e]
                    dict set components $name exclude [list [list sym] [format {if {$sym in {%s}} list {set sym}} $e]]]
                }
                -insert {
                    set args [lassign $args - i]
                    dict set components $name insert [list [list sym] [format {my add %s $sym} $i] [self namespace]]
                }
                -scalar {
                    set args [lassign $args -]
                    dict set components $name scalar 1
                }
                -domain {
                    set args [lassign $args - d]
                    dict set components $name value {0 1}
                    dict set components $name domain $d
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

    method id {names as} {
        if {[dict exists $components id]} {
            return -code error [format {id format already defined}]
        }
        set name id
        dict set components $name label ID
        dict set components $name type id
        dict set components $name names $names
        dict set components $name as $as
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
            default {
                my AddGraded $what {*}$args
            }
        }
    }

    method AddGraded {name args} {
        if {[llength $args] > 0} {
            if {[my FitsGraded $name syms {*}$args]} {
                my AddGradedValue $name $syms
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
                set value [lsort -unique -dict $value]
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
                        return
                    }
                } else {
                    if {[my FitsGraded $fmt sym $arg]} {
                        my AddGradedValue $fmt $sym
                        lappend tuple [lindex $sym 0]
                    } else {
                        return
                    }
                }
            }
            lappend value $tuple
        }
    }

    method AddID args {
        set name id
        set result [dict create]
        dict with components $name {
            foreach arg $args key $names fmt $as {
                if {$key eq {} || $fmt eq {}} {
                    return -code error [format {too many symbols}]
                }
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

}
