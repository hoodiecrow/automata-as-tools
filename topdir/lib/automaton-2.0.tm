
oo::class create ::FSM {
    variable transitionMatrix accepting start

    constructor args {
        lassign $args transitionMatrix accepting start
    }

    method start s {
        set start $s
    }

    method set {key nstate} {
        dict lappend transitionMatrix $key $nstate
    }

    method trans key {
        if {[dict exists $transitionMatrix $key]} {
            log::log d [list $key -> [dict get $transitionMatrix $key]]
            dict get $transitionMatrix $key
        }
    }

    method accepting args {
        set accepting $args
    }

    method accept states {
        foreach state $states {
            if {$state in $accepting} {return 1}
        }
        return 0
    }

    method nstates {states input} {
        set ns [lmap state $states {
            set t [my trans [list $state $input]]
            if {$t eq {}} {
                if {$input eq {}} {
                    list $state
                } else {
                    if no {
                        return -code error [format {NOTRANS "%s" "%s"} $state $input]
                    }
                }
            } else {
                set t
            }
        }]
        set ns [lsort -unique [concat {*}$ns]]
        if {{} in $ns} {
            return [lassign $ns -]
        } else {
            return $ns
        }
    }

    method run inputs {
        set states [list $start]
        foreach input $inputs {
            set states [my nstates [my nstates $states {}] $input]
            if {[llength $states] < 1} {
                return 0
            }
        }
        set states [my nstates $states {}]
        return [my accept $states]
    }

}

oo::class create ::TM {
    superclass ::FSM

    variable start tape position output halting

    constructor args {
        set args [lassign $args tape position]
        set output {}
        set halting {}
        next {} {*}$args
    }

    method halting args {
        set halting $args
    }

    method halt states {
        foreach state $states {
            if {$state in $halting} {return 1}
        }
        return 0
    }

    method set {key nstate} {
        # st = stack top
        # sp = stack push
        lassign $key state input
        lassign [split $input /] input move
        lassign [split $move \;] symbol direction
        set key [list $state $input]
        next $key $nstate
        dict set output $key [list $symbol $direction]
    }

    method trans key {
        set t [next $key]
        if {$t ne {}} {
            lassign [dict get $output $key] symbol direction
            log::log d \$tape=[lreplace $tape $position $position *],\ \$symbol=$symbol,\ \$direction=$direction
            lset tape $position $symbol
            switch $direction {
                L { incr position -1 }
                R { incr position 1 }
            }
            # TODO check boundaries / extend tape
        }
        return $t
    }

    method run {} {
        set states [list $start]
        set input [lindex $tape $position]
        while {$input ne {}} {
            set states [my nstates [my nstates $states {}] $input]
            if {[llength $states] < 1} {
                return
            }
            if {[my halt $states]} {
                break
            }
            set input [lindex $tape $position]
        }
        set states [my nstates $states {}]
        return [my tape]
    }

    method tape {} {
        return $tape
    }

}

oo::class create ::PDA {
    superclass ::FSM

    variable stack stackStart stackPush

    constructor args {
        set stack {}
        set stackPush {}
        next {} {*}$args
    }

    method set {key nstate} {
        # st = stack top
        # sp = stack push
        lassign $key state input
        lassign [split $input \;] input st
        lassign [split $st /] st sp
        set key [list $state $input $st]
        next $key $nstate
        dict set stackPush $key [split $sp {}]
    }

    method stack symbol {
        set stackStart [list $symbol]
    }

    method trans key {
        lassign $key state input
        lassign $stack s
        set key [list $state $input $s]
        set t [next $key]
        if {$t ne {}} {
            set stack [lreplace $stack 0 0 {*}[dict get $stackPush $key]]
        }
        return $t
    }

    method run args {
        set stack $stackStart
        next {*}$args
    }

}

oo::class create ::FST {
    superclass ::FSM

    variable output soutput toutput

    constructor args {
        set output {}
        set soutput {}
        set toutput {}
        next {} {*}$args
    }

    method set {key nstate} {
        # to = transition-based output
        # so = state-based output
        lassign $key state input
        lassign [split $state /] state so
        lassign [split $input /] input to
        next [list $state $input] $nstate
        if {$so ne {}} {
            dict set soutput $state $so
        }
        if {$to ne {}} {
            dict set toutput [list $state $input] $to
        }
    }

    method trans key {
        lassign $key state input
        set t [next $key]
        if {$input ne {}} {
            if {[dict exists $soutput $state]} {
                lappend output [dict get $soutput $state]
            }
            if {$t ne {}} {
                if {[dict exists $toutput $key]} {
                    lappend output [dict get $toutput $key]
                }
            }
        }
        return $t
    }

    method run args {
        set output {}
        next {*}$args
    }

    method output {} {
        return $output
    }

}
