package require json
package require json::write

apply {args {
    set dir [file dirname [info script]]
    foreach arg $args {
        source -encoding utf-8 [file join $dir .. src $arg]
    }
}} set.tcl

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

# tuples
# DFSA S I   td    s   A
# NFSA S I   td    s   A
# DFST S I O td od s   A
# NFST S I O td od s   A
# DPDA S I Z td    s z A
#
# *FST modes
# recognizer: accepts when the second tape is a relation of the first
# generator: walk the graph, outputting to both tapes
# translation: creates the second tape according to the first
# rtranslation: creates the first tape according to the second
#
# 1 "a b" 1
# recognize {a a a} {b b b} -> 1
# generate 3 -> {a a a} {b b b}
# translate {a a a} -> {b b b}
# reconstruct {b b b} -> {a a a}
#
# 1 "a a" 2
# 2 "ε a" 1
# recognize {a a} {a a a a} -> 1
# generate 2 -> {a a} {a a a a}
# translate {a a} -> {a a a a}
# reconstruct {a a a a} -> {a a}

oo::class create DFST {
    variable tuple od td kind inputs outputs
    constructor args {
        lassign {} od td
        lassign $args tuple transitions kind
        if {[dict exists $tuple td]} {
            set td [dict get $tuple td]
        }
        if {[dict exists $tuple od]} {
            set od [dict get $tuple od]
        }
        foreach {from edge next} $transitions {
            if {[llength $from] > 1 && [llength $edge] == 1} {
                set x [lassign $from s]
                set e $edge
                dict lappend od $s $x
                set kind moore
            } elseif {[llength $from] == 1 && [llength $edge] > 1} {
                set s $from
                set y [lassign $edge e]
                if {$e eq "ε"} {
                    set e {}
                }
                if {![dict exists $od $s $e]} {
                    dict set od $s $e {}
                }
                dict with od $s {
                    lappend $e $y
                }
                set kind mealy
            } else {
                return -code error [format {can't build output dictionary from both state and edge}]
            }
            if {![dict exists $td $s $e]} {
                dict set td $s $e {}
            }
            dict with td $s {
                lappend $e $next
            }
        }
    }
    method ProduceOutput {varName input {ia {}}} {
        upvar 1 $varName state
        if {[dict exists $td $state $input]} {
            eval $ia
            if {$kind eq "moore"} {
                lappend outputs {*}[dict get $od $state]
            } else {
                lappend outputs {*}[dict get $od $state $input]
            }
            set state [dict get $td $state $input]
        }
    }
    method ConsumeOutput {varName input {ia {}}} {
        upvar 1 $varName state
        if {[dict exists $td $state $input]} {
            if {$kind eq "moore"} {
                foreach o [dict get $od $state] {
                    set outputs [lassign $outputs output]
                    if {$o ne $output} {
                        return fail
                    }
                }
            } else {
                foreach o [dict get $od $state $input] {
                    set outputs [lassign $outputs output]
                    if {$o ne $output} {
                        return fail
                    }
                }
            }
            eval $ia
            set state [dict get $td $state $input]
        }
    }
    method generate n {
        set state [dict get $tuple s]
        lassign {} inputs outputs
        while {[incr n -1] >= 0} {
            my ProduceOutput state {}
            set input [lindex [dict get $td $state] 0]
            my ProduceOutput state $input {lappend inputs $input}
        }
        my ProduceOutput state {}
        list $inputs $outputs
    }
    method recognize args {
        set state [dict get $tuple s]
        lassign $args inputs outputs
        foreach input $inputs {
            my ConsumeOutput state {}
            if {[llength $outputs] eq 0} {
                break
            }
            my ConsumeOutput state $input
            if {[llength $outputs] eq 0} {
                break
            }
        }
        my ConsumeOutput state {}
        if {[llength $outputs] > 0} {
            return 0
        }
        expr {$state in [dict get $tuple A]}
    }
    method translate args {
        set state [dict get $tuple s]
        lassign $args inputs outputs
        foreach input $inputs {
            my ProduceOutput state {}
            my ProduceOutput state $input
        }
        my ProduceOutput state {}
        if {$state in [dict get $tuple A]} {
            return $outputs
        } else {
            return fail
        }
    }
    method reconstruct args {
        set state [dict get $tuple s]
        lassign [linsert $args 0 {}] inputs outputs
        while {[llength $outputs] > 0} {
            my ConsumeOutput state {}
            if {[llength $outputs] eq 0} {
                break
            }
            set input [lindex [dict get $td $state] 0]
            my ConsumeOutput state $input {lappend inputs $input}
        }
        my ConsumeOutput state {}
        if {[llength $outputs] > 0} {
            return fail
        }
        if {$state in [dict get $tuple A]} {
            return $inputs
        } else {
            return fail
        }
    }
}

return
oo::class create DFST {
    variable state S I O td od s A

    constructor args {
        lassign $args S I O td od s A

        oo::objdefine [self] forward tf dict get $td
        oo::objdefine [self] forward of dict get $od
    }

    method _tf args {
        dict get $td {*}$args
    }

    method _of args {
        dict get $od {*}$args
    }

    method accept a {
        expr {$a in $A}
    }

    method exec inputs {
        set result {}
        set state $s
        foreach input $inputs {
            log::log d \$state=$state,\ \$input=$input
            log::log d "transition = [dict get $td $state $input]"
            log::log d "output     = [dict get $od $state $input]"
            lappend result {*}[my of $state $input]
            set state [my tf $state $input]
        }
        if {[my accept $state]} {
            return $result
        } else {
            return fail
        }
    }
}

return
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
