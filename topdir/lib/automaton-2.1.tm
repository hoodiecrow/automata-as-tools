
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
    # A H I O S T Z: list values
    # b s z:         single values
    # o t:           list of associations
    # A accepting states
    # H halting states
    # I input alphabet
    # O output alphabet
    # S state symbols
    # T tape alphabet
    # Z stack symbols
    # b blank tape symbol
    # s starting state
    # z starting stack symbol
    # o output function
    # t transition function
    set result {}
    dict for {k v} $tuple {
        switch $k {
            A - H - I - O - S - T -
            Z { dict set result $k [::json::write array {*}[lmap i $v {::json::write string $i}]] }
            b - s -
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
