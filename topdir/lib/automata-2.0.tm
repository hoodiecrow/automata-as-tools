
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
    variable data

    constructor args {
        foreach arg $args {
            lassign $arg key value
            my set $key $value
        }
    }

    method unset key {
        dict unset data $key
    }

    method set {key {value {}}} {
        dict set data $key $value
    }

    method get key {
        if {[dict exists $data $key]} {
            dict get $data $key
        }
    }

    method add {keys values} {
        foreach key $keys value $values {
            if {$value ni [dict get $data $key] && $value ne {}} {
                dict lappend data $key $value
            }
        }
    }

    method exists key {
        dict exists $data $key
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
    variable transitionMatrix

    constructor args {
        lassign $args transitionMatrix acceptingItems start
        Set create states
        Tuple create tuple I S \
            [list t $transitionMatrix] \
            [list s $start] \
            [list A $acceptingItems]
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
            log::log i [list $key -> [dict get $transitionMatrix $key]]
            dict get $transitionMatrix $key
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
        states set [list [tuple get s]]
        foreach input $inputs {
            states add [my moves {}]
            states set [my moves $input]
            if {[states empty]} {
                return 0
            }
        }
        states add [my moves {}]
        return [states intersects [tuple get A]]
    }

}

oo::class create ::TM {
    superclass ::FSM

    variable start tape position output

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
            log::log i \$tape=[lreplace $tape $position $position *],\ \$symbol=$symbol,\ \$direction=$direction
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
        states set [list [tuple get s]]
        set input [lindex $tape $position]
        while {$input ne {}} {
            states add [my moves {}]
            states set [my moves $input]
            if {[states empty]} {
                return
            }
            if {[states intersects [tuple get H]]} {
                return $tape
            }
            set input [lindex $tape $position]
        }
        states add [my moves {}]
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

    method run args {
        set output {}
        next {*}$args
    }

    method output {} {
        return $output
    }

}
