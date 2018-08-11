
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
    variable tuple

    constructor args {
        lassign $args tuple
        Set create states
    }

    method trans key {
        if {[dict exists $tuple t $key]} {
            log::log i [list $key -> [dict get $tuple t $key]]
            dict get $tuple t $key
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
            return [states intersects [dict get $tuple A]]
        }
    }

    method show {} {
        set output {}
        dict for {k v} $tuple {
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

    variable tuple stack

    constructor args {
        next {*}$args
    }

    method Push args {
        set stack [lreplace $stack 0 0 {*}$args]
    }

    method Top {} {
        lindex $stack 0
    }

    method trans key {
        lassign $key state input
        log::log d \$state=$state 
        log::log d \$input=$input 
        set s [my Top]
        set key [list $state $input $s]
        log::log d \$key=$key 
        set t [next $key]
        log::log d \$t=$t 
        if {$t ne {}} {
            log::log d "push=[dict get $tuple o $key]"
            my Push {*}[dict get $tuple o $key]
        }
        return $t
    }

    method run args {
        set stack [list [dict get $tuple z]]
        next {*}$args
    }

}

oo::class create ::FST {
    superclass ::FSM

    variable tuple output

    constructor args {
        next {*}$args
    }

    method trans key {
        lassign $key state input
        set t [next $key]
        if {$input ne {}} {
            if {[dict exists $tuple o $state]} {
                lappend output [dict get $tuple o $state]
            }
            if {$t ne {}} {
                if {[dict exists $tuple o $key]} {
                    lappend output [dict get $tuple o $key]
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
