package require struct::matrix

namespace eval automata {}

oo::class create ::automata::Machine3 {

    constructor args {
        next {*}$args
    }

    method search {id fn {steps {}}} {
        if {$steps ne {}} {
            if {$steps <= 0} {
                return [list $id]
            } else {
                incr steps -1
            }
        }
        set ids [my $fn $id]
        if {[llength $ids] eq 0} {
            return [list $id]
        }
        set ids [lsort -unique $ids]
        return [concat {*}[lmap id $ids {
            my search $id $fn $steps
        }]]
    }

    method consumeOne id {
        # unpack ID
        dict with id {
            # get epsilons
            set targets [lmap row [my get table $q {}] {
                lindex $row 2
            }]
            set ids [lmap target $targets {
                my add id $a $target
            }]
            set _a [lassign $a A]
            set targets [lmap row [my get table $q $A] {
                lindex $row 2
            }]
            lappend ids {*}[lmap target $targets {
                my add id $_a $target
            }]
        }
        return $ids
    }

    method recognize id {
        # unpack ID
        dict with id {
            # get epsilons
            set tuples [my get table $q {}]
            set _a [lassign $a A]
            set _b [lassign $b B]
            # get moves
            lappend tuples {*}[my get table $q $A]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # consume input token
                    lset tuple 1 $_a
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } elseif {$out ne $B} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $_b
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method translate id {
        # unpack ID
        dict with id {
            set _a [lassign $a A]
            # get epsilons
            set tuples [my get table $q {}]
            # get moves
            lappend tuples {*}[my get table $q $A]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # consume input token
                    lset tuple 1 $_a
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } else {
                    # emit output token
                    lset tuple 3 [linsert $b end [lindex $out 0]]
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method reconstruct id {
        # unpack ID
        dict with id {
            set _b [lassign $b B]
            # get moves
            set tuples [my get table $q *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # emit input token
                    lset tuple 1 [linsert $a end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } elseif {$out ne $B} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $_b
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method generate id {
        # unpack ID
        dict with id {
            # get moves
            set tuples [my get table $q *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $a
                } else {
                    # emit input token
                    lset tuple 1 [linsert $a end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $b
                } else {
                    # emit output token
                    lset tuple 3 [linsert $b end [lindex $out 0]]
                }
                my add id {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method makeMoves id {
        # unpack ID
        dict with id {
            set _w [lassign $w W]
            set _z [lassign $z Z]
            # get epsilons
            set tuples [my get table $q {}]
            # get moves
            lappend tuples {*}[my get table $q $W]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 O _o
                if {$inp eq {}} {
                    lset tuple 1 $w
                } else {
                    # consume input token
                    lset tuple 1 $_w
                }
                if {$O ne $Z} {
                    # reject invalid transition
                    continue
                } else {
                    # push stack
                    lset tuple 3 [concat {*}$_o $_z]
                }
                my add id {*}[apply {tuple {
                    set _z [lassign $tuple - w q z]
                    list $w $q $z
                }} $tuple]
            }]
        }
        return $ids
    }

    #: In tape machines, data is in a sequential-accessed sequence that can
    #: grow if new elements are added at the ends.
    #:
    #: The operations supported are:
    #:
    #: Print
    #:  <symbol> print symbol (including blank)
    #:  N        do not print
    #:
    #: Move
    #:  L        move tape one cell to the left
    #:  R        move tape one cell to the right
    #:  N        do not move tape

    method Print {varName h p} {
        upvar 1 $varName tape
        switch $p {
            N  {}
            E  { lset tape $h [lindex [my get A] 0] }
            P  { lset tape $h [lindex [my get A] 1] }
            default {
                if {[regexp {^P(.)$} $p -> s]} {
                    lset tape $h $s
                }
            }
        }
        return
    }

    method Move {varName1 varName2 dir} {
        upvar 1 $varName1 tape $varName2 h
        switch $dir {
            L {
                incr h
                if {$h >= [expr {[llength $tape] - 1}]} {
                    lappend tape [lindex [my get A] 0]
                }
            }
            R {
                if {$h < 1} {
                    set tape [linsert $tape 0 [lindex [my get A] 0]]
                } else {
                    incr h -1
                }
            }
            N {}
        }
        return
    }

    method process id {
        # unpack ID
        dict with id {
            if {[my in F $q]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [my get table $q [lindex $t $h]]
            set ids [lmap tuple $tuples {
                lassign $tuple - - q1 p m
                my Print t $h $p
                my Move t h $m
                my add id $t $h $q1
            }]
        }
        return $ids
    }

    method ALU {op data args} {
        # Shared between KTR and SM
        switch $op {
            INC { set res [expr {[lindex $data {*}$args] + 1}] }
            DEC { set res [expr {[lindex $data {*}$args] - 1}] }
            CLR { set res 0 }
            default {
                if {[string is upper -strict $op]} {
                    set op [dict get {
                        EQ  eq
                        EQL ==
                        ADD +
                        MUL *
                    } $op]
                }
                set res [::tcl::mathop::$op {*}[lmap arg $args {
                    lindex $data $arg
                }]]
            }
        }
        if {$res < 0} {
            return -code error [format {result less than 0}]
        }
        return $res
    }

}

oo::class create ::automata::Type {
    variable desc type options

    constructor args {
        array set options [lassign $args desc type]
    }

}

oo::class create ::automata::Types {
    constructor args {
        ::struct::matrix matrix
        matrix add columns 5
    }

    forward matrix matrix

    method addrow {name desc type vals args} {
        log::log d [info level 0] 
        set args [dict merge {
            -sorted 1
            -epsilon {}
            -index -1
            -hidden 0
        } $args]
        my matrix add row [list $name $type $vals $args $desc]
    }

    method getrow name {
        log::log d [info level 0] 
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
        log::logarray d options
        if {$symbol eq {}} {
            return -code error [format {empty symbol}]
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
 
oo::class create ::automata::Machine {
    mixin ::automata::Machine3

    variable types

    constructor args {
        set types [::automata::Types new]
        next {*}$args
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

    method settype {name symbol} {
        $types set $name $symbol
    }

}

