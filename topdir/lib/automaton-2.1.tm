
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

    forward output set result

    method action {transitions key} {
        return $transitions
    }

    method trans key {
        if {[dict exists $tuple t $key]} {
            log::log i [list $key -> [dict get $tuple t $key]]
            return [my action [dict get $tuple t $key] $key]
        } else {
            return {}
        }
    }

    method moves input {
        set ns [states map state {
            my trans [list $state $input]
        }]
        set res [$ns all]
        $ns destroy
        return $res
    }

    method run inputs {
        set result {}
        states set [list [dict get $tuple s]]
        foreach input $inputs {
            states add [my moves {}]
            states set [my moves $input]
            if {[states empty]} {
                return 0
            }
        }
        states add [my moves {}]
        if {[dict exists $tuple A]} {
            my output [states intersects [dict get $tuple A]]
        }
        return $result
    }

    method show {} {
        set output {}
        dict for {k v} $tuple {
            switch $k {
                A - H - I - O - S - T -
                Z { dict set output $k [::json::write array {*}[lmap i $v {::json::write string $i}]] }
                b - s -
                z { dict set output $k [::json::write string $v] }
                o -
                t {
                    set t {}
                    dict for {key val} $v {
                        lappend t $key [::json::write array {*}[lmap i $val {::json::write string $i}]]
                    }
                    dict set output $k [::json::write object {*}$t]
                }
                default {
                    return -code error [format {unknown tuple key "%s"} $k]
                }
            }
        }
        return [::json::write object {*}$output]
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

    method action {transitions key} {
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
            states set [my moves [lindex $tape $position]]
            if {[states intersects [dict get $tuple H]]} {
                break
            }
        }
        return $tape
    }

}

oo::class create ::PDA {
    superclass ::FSM

    variable tuple stack

    method action {transitions key} {
        if {$transitions ne {}} {
            set stack [lreplace $stack 0 0 {*}[dict get $tuple o $key]]
        }
        return $transitions
    }

    method trans key {
        lappend key [lindex $stack 0]
        return [my action [next $key] $key]
    }

    method run args {
        set stack [list [dict get $tuple z]]
        next {*}$args
    }

}

oo::class create ::FST {
    superclass ::FSM

    variable tuple result

    forward output lappend result

    method action {transitions key} {
        lassign $key state input
        if {$input ne {}} {
            # TODO regulate output action (state entry, state exit, transition)
            if {[dict exists $tuple o $state]} {
                my output [dict get $tuple o $state]
            }
            if {$transitions ne {}} {
                if {[dict exists $tuple o $key]} {
                    my output [dict get $tuple o $key]
                }
            }
        }
        return $transitions
    }

}
