
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

    method size {} {
        dict size $data
    }

    method all {} {
        dict keys $data
    }

    method exists key {
        dict exists $data $key
    }

    method intersect other {
        set res [Set new]
        if {[dict size $data] > [$other size]} {
            dict for {k -} $data {
                if {[$other exists $k]} {
                    $res add $k
                }
            }
        } else {
            $other for k {
                if {[dict exists $data $k]} {
                    $res add $k
                }
            }
        }
        return $res
    }

    method for {varName script} {
        upvar 1 $varName var
        foreach var [dict keys $data] {
            uplevel 1 $script
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

oo::class create Tuple {
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
    variable data machine

    constructor args {
        my setKeys [lassign $args machine]
    }

    method setKeys keys {
        foreach key $keys {
            dict set data $key {}
        }
    }

    method unset key {
        dict unset data $key
    }

    method set {key {value {}}} {
        dict set data $key $value
    }

    method get key {
        dict get $data $key
    }

    method add {keys values} {
        foreach key $keys value $values {
            if {$value ni [dict get $data $key] && $value ne {}} {
                dict lappend data $key $value
            }
        }
    }

    method show {} {
        set output {}
        dict for {k v} $data {
            switch $k {
                A - H - I - O - S - T -
                Z { append output $k=[list [join $v {, }]]\n }
                b - s -
                z { append output $k=$v\n }
                o -
                t {
                    set t {}
                    join [dict for {key val} $v {
                        lappend t [list [list $key]->[list $val]]
                    }] {, }
                    append output $k=[list [join $t {, }]]\n
                }
                default {
                    return -code error [format {unknown tuple key "%s"} $k]
                }
            }
        }
        return $output
    }

}

oo::class create ::FSM {
    variable transitionMatrix states

    constructor args {
        lassign $args transitionMatrix acceptingItems start
        set states [Set new]
        Tuple create tuple [self] I S t
        tuple set s $start
        tuple set A $acceptingItems
    }

    forward tuple tuple

    method Key key {
        # Should only be called by the 'set' method.
        # Override this method to do any preprocessing of the key value
        # and/or conduct any side processing.
        tuple add {S I} $key
        set key
    }

    method set {key nstate} {
        dict lappend transitionMatrix [my Key $key] $nstate
        tuple set t $transitionMatrix
    }

    method trans key {
        if {[dict exists $transitionMatrix $key]} {
            log::log d [list $key -> [dict get $transitionMatrix $key]]
            dict get $transitionMatrix $key
        }
    }

    method accept {} {
        set accepting [Set new {*}[tuple get A]]
        set int [$states intersect $accepting]
        set res [expr {[$int size] > 0}]
        $int destroy
        $accepting destroy
        return $res
    }

    method nstates input {
        set ns [$states map state {
            set t [my trans [list $state $input]]
            if {$t eq {} && $input eq {}} {
                list $state
            } else {
                set t
            }
        }]
        $states set [$ns all]
        $ns destroy
        return
    }

    method run inputs {
        $states set [list [tuple get s]]
        foreach input $inputs {
            my nstates {}
            my nstates $input
            if {[$states size] < 1} {
                return 0
            }
        }
        my nstates {}
        return [my accept]
    }

}

oo::class create ::TM {
    superclass ::FSM

    variable start tape position output states

    constructor args {
        set args [lassign $args tape position]
        set output {}
        next {} {*}$args
        foreach key {o T H} {
            tuple set $key
        }
        tuple set b _
    }

    method Key key {
        # st = stack top
        # sp = stack push
        lassign $key state input
        lassign [split $input /] input move
        lassign [split $move \;] symbol direction
        set key [list $state $input]
        dict set output $key [list $symbol $direction]
        tuple add {S I} $key
        tuple add T $input
        tuple add T $symbol
        tuple set o $output
        return $key
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
        $states set [list [tuple get s]]
        set input [lindex $tape $position]
        while {$input ne {}} {
            my nstates {}
            my nstates $input
            if {[$states size] < 1} {
                return
            }
            set halting [Set new {*}[tuple get H]]
            set int [$states intersect $halting]
            set res [expr {[$int size] > 0}]
            $int destroy
            $halting destroy
            if {$res > 0} {
                break
            }
            set input [lindex $tape $position]
        }
        my nstates {}
        return $tape
    }

}

oo::class create ::PDA {
    superclass ::FSM

    variable theStack stackPush

    constructor args {
        set theStack {}
        set stackPush {}
        next {} {*}$args
        foreach key {Z z} {
            tuple set $key
        }
    }

    method Push args {
        set theStack [lreplace $theStack 0 0 {*}$args]
    }

    method Top {} {
        lindex $theStack 0
    }

    method Key key {
        # st = stack top
        # sp = stack push
        lassign $key state input
        lassign [split $input \;] input st
        lassign [split $st /] st sp
        set key [list $state $input $st]
        dict set stackPush $key [split $sp {}]
        tuple add {S I Z} $key
        return $key
    }

    method trans key {
        lassign $key state input
        set s [my Top]
        set key [list $state $input $s]
        set t [next $key]
        if {$t ne {}} {
            my Push {*}[dict get $stackPush $key]
        }
        return $t
    }

    method run args {
        my Push [tuple get z]
        next {*}$args
    }

}

oo::class create ::FST {
    superclass ::FSM

    variable output soutput toutput

    constructor args {
        lassign {} output soutput toutput
        next {} {*}$args
        foreach key {O o} {
            tuple set $key
        }
        tuple unset A
    }

    method Key key {
        # to = transition-based output
        # so = state-based output
        lassign $key state input
        lassign [split $state /] state so
        lassign [split $input /] input to
        set key [list $state $input]
        if {$so ne {}} {
            tuple add O $so
            dict set soutput $state $so
        }
        if {$to ne {}} {
            tuple add O $to
            dict set toutput [list $state $input] $to
        }
        tuple add {S I} $key
        return $key
    }

    method set args {
        next {*}$args
        tuple set o [dict merge $soutput $toutput]
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

    method accept {} {return 1}

    method run args {
        set output {}
        next {*}$args
    }

    method output {} {
        return $output
    }

}
