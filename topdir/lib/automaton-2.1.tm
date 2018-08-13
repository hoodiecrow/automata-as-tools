
package require json
package require json::write

oo::class create Set {
    variable data

    constructor args {
        my set $args
    }

    method set values {
        set data {}
        my add $values
    }

    method add values {
        foreach value $values {
            if {$value ne {}} {
                dict set data $value 1
            }
        }
    }

    method empty {} {
        expr {[my size] == 0}
    }

    method size {} {
        dict size $data
    }

    method all {} {
        dict keys $data
    }

    method exists key {
        dict exists $data $key
    }

    method intersects stateset {
        my for s {
            if {$s in $stateset} {
                return 1
            }
        }
        return 0
    }

    # TODO might need to cover more return codes in for and map
    method for {varName script} {
        upvar 1 $varName var
        foreach var [dict keys $data] {
            try {
                uplevel 1 $script
            } on return res {
                return -level 2 $res
            }
        }
    }

    method map {varName script} {
        upvar 1 $varName var
        set res [Set new]
        foreach var [dict keys $data] {
            $res add [uplevel 1 $script]
        }
        return $res
    }

}

proc tuple2json tuple {
    # tuple keys:
    # A C H I O S T Z: list values
    # b s z:           single values
    # o t:           list of associations
    # A accepting states
    # C control codes ("program")
    # H halting states
    # I input alphabet
    # O output alphabet
    # S state symbols
    # T tape alphabet
    # Z stack symbols
    # b blank tape symbol
    # m 'mark' symbol
    # s starting state
    # z starting stack symbol
    # o output function
    # t transition function
    set result {}
    dict for {k v} $tuple {
        switch $k {
            A - C - H - I - O - S - T -
            Z { dict set result $k [::json::write array {*}[lmap i $v {::json::write string $i}]] }
            b - m - s -
            z { dict set result $k [::json::write string $v] }
            o -
            t {
                set t {}
                dict for {key val} $v {
                    lappend t $key [::json::write array {*}[lmap i $val {::json::write string $i}]]
                }
                dict set result $k [::json::write object {*}$t]
            }
            default {
                return -code error [format {unknown tuple key "%s"} $k]
            }
        }
    }
    return [::json::write object {*}$result]
}

proc assemble items {
    set map {}
    set code {}
    set n 0
    foreach item $items {
        if {[string match *: $item]} {
            lappend map [string trimright $item :] $n
        } elseif {[regexp {^(J[01]?):\*([+-]\d+)$} $item -> op offset]} {
            lappend code $op [expr $n$offset]
        } else {
            lappend code {*}[split $item :]
            set n [llength $code]
        }
    }
    string map $map $code
}

oo::class create ::FSM {
    variable tuple result

    constructor args {
        if {[string equal -nocase [lindex $args 0 0] json]} {
            set tuple [::json::json2dict [lindex $args 0 1]]
        } else {
            lassign $args tuple
        }
        Set create states
    }

    forward Output set result

    method Action {transitions key} {
        return $transitions
    }

    method Trans key {
        if {[dict exists $tuple t $key]} {
            set t [dict get $tuple t $key]
            log::log i [list $key -> $t]
            return [my Action $t $key]
        } else {
            return {}
        }
    }

    method Moves input {
        set ns [states map state {
            my Trans [list $state $input]
        }]
        set res [$ns all]
        $ns destroy
        return $res
    }

    method run inputs {
        set result {}
        states set [list [dict get $tuple s]]
        foreach input $inputs {
            states add [my Moves {}]
            states set [my Moves $input]
            if {[states empty]} {
                return 0
            }
        }
        states add [my Moves {}]
        if {[dict exists $tuple A]} {
            my Output [states intersects [dict get $tuple A]]
        }
        return $result
    }

    method tuple {} {
        return $tuple
    }

}

oo::class create ::FST {
    superclass ::FSM

    variable tuple result

    forward Output lappend result

    method Action {transitions key} {
        lassign $key state input
        if {$input ne {}} {
            # TODO regulate output action (state entry, state exit, transition)
            if {[dict exists $tuple o $state]} {
                my Output [dict get $tuple o $state]
            }
            if {$transitions ne {}} {
                if {[dict exists $tuple o $key]} {
                    my Output [dict get $tuple o $key]
                }
            }
        }
        return $transitions
    }

}

oo::class create ::PDA {
    superclass ::FSM

    variable tuple stack

    method Action {transitions key} {
        if {$transitions ne {}} {
            set stack [lreplace $stack 0 0 {*}[dict get $tuple o $key]]
        }
        return $transitions
    }

    method Trans key {
        lappend key [lindex $stack 0]
        return [my Action [next $key] $key]
    }

    method run args {
        set stack [list [dict get $tuple z]]
        next {*}$args
    }

}

oo::class create ::DTM {
    superclass ::FSM

    variable tuple tape position

    constructor args {
        next {*}$args
        set tape [list [dict get $tuple b]]
        set position 0
    }

    method Action {transitions key} {
        if {$transitions ne {}} {
            lassign [dict get $tuple o $key] symbol direction
            log::log i \$tape=[lreplace $tape $position $position *],\ \$symbol=$symbol,\ \$direction=$direction
            lset tape $position $symbol
            switch $direction {
                L {
                    # move tape left
                    incr position
                    lappend tape [dict get $tuple b]
                }
                R {
                    # move tape right
                    set tape [linsert $tape 0 [dict get $tuple b]]
                }
            }
        }
        return $transitions
    }

    method run {} {
        states set [list [dict get $tuple s]]
        while {![states empty]} {
            states set [my Moves [lindex $tape $position]]
            if {[states intersects [dict get $tuple H]]} {
                break
            }
        }
        return $tape
    }


}

oo::class create ::PTM {

    variable tuple tape position control

    constructor args {
        lassign $args tuple tape position
        if {[string equal -nocase [lindex $tuple 0] json]} {
            set tuple [::json::json2dict [lindex $tuple 1]]
        } else {
            lassign $args tuple
        }
        if {$tape eq {}} {
            set tape [list [dict get $tuple b]]
            set position 0
        }
        dict set tuple C [assemble [dict get $tuple C]]
        set control 0
    }

    method Action op {
        set b [dict get $tuple b]
        set m [dict get $tuple m]
        set input [lindex $tape $position]
        set operations [dict get $tuple C]
        switch $op {
            E {
                lset tape $position $b
                incr control
            }
            P {
                lset tape $position $m
                incr control
            }
            L {
                # move head left
                if {$position > 0} {
                    incr position -1
                } else {
                    set tape [linsert $tape 0 $b]
                }
                incr control
            }
            R {
                # move head right
                incr position
                if {$position >= [llength $tape]} {
                    lappend tape $b
                }
                incr control
            }
            H {}
            J {
                set control [lindex $operations [incr control]]
            }
            J0 {
                if {$input eq $b} {
                    set control [lindex $operations [incr control]]
                } else {
                    incr control 2
                }
            }
            J1 {
                if {$input eq $m} {
                    set control [lindex $operations [incr control]]
                } else {
                    incr control 2
                }
            }
            default {
                return -code error [format {unknown operation "%s"} $op]
            }
        }
        return $op
    }

    method run {} {
        while {![info exists op] || $op ni [dict get $tuple H]} {
            set op [my Action [lindex [dict get $tuple C] $control]]
        }
        return $tape
    }

    method tuple {} {
        return $tuple
    }

}
