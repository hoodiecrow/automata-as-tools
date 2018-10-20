
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

namespace eval automata {}

proc ::tcl::mathfunc::cmp {a b} { if {$a == $b} {return 0} elseif {$a < $b} {return -1} else {return 1}}

oo::class create ::automata::Processor {
    variable data machine
    constructor args {
        lassign $args _model machine
        set data {
            model {}
            acc 0
            aux 0
            jmp {}
            <=> 0
            returns {}
            registers {}
            world {}
            robot {}
            tape {}
            head 0
            stack {}
            ipointer 0
        }
        dict set data model $_model
    }
    method acc: val {
        log::log d [info level 0] 
        dict with data {
            set acc $val
            set <=> [expr {cmp($val, 0)}]
        }
    }
    method :cflag {} { dict with data { expr {${<=>} > 0} }}
    method :zflag {} { dict with data { expr {${<=>} eq 0} }}
    method <=>: val {}
    export <=>:
    method returns: addr {
        dict with data {
            set returns [linsert $returns 0 $addr]
        }
    }
    method :returns {} {
        dict with data {
            set returns [lassign $returns addr]
        }
        return $addr
    }
    export :returns
    method registers: args { dict with data { lset registers {*}$args } }
    method :registers args { lindex [dict get $data registers] {*}$args }
    export :registers
    method stack: args { dict with data { lset stack {*}$args } }
    method :stack {} { lindex [dict get $data stack] 0 }
    export :stack
    method tape: val { dict with data { lset tape $head $val } }
    method :tape {} { dict with data { lindex $tape $head } }
    export :tape
    method load args {
        if {[lindex $args 1] eq "tape"} {
            my acc: [my :tape]
        } else {
            switch [my :model] {
                CM {
                    lassign $args - b
                    my acc: [my :registers $b]
                }
                PTM {
                    my acc: [my :tape]
                }
                SM {
                    my top
                }
            }
        }
    }
    method top {} { dict with data { lindex $stack 0 } }
    method pop {} { dict with data { set stack [lassign $stack acc] } }
    method popx {} { dict with data { set stack [lassign $stack aux] } }
    method push {} { dict with data { set stack [linsert $stack 0 $acc] } }
    method swap {} {
        dict with data {
            set val $acc
            set acc $aux
            set aux $val
        }
    }
                    
    method unknown {name args} {
        set key [string trim $name :]
        if {$key in [dict keys $data]} {
            switch $name $key: {
                dict set data $key {*}$args
            } :$key {
                dict get $data $key
            }
        } elseif {$key in {
                CLR DUP CPY PUSH POP INC DEC
                JMP JE JNE JZ JNZ CALL RET
                NOP HALT OUT TEST
        }} {
            my $key {*}$args
        } elseif {$key in {
                EQ NE EQL NEQ GT GE LT LE
        }} {
            my OP $key {*}$args
        } elseif {$key in {
                ADD SUB MUL DIV MOD
                AND OR XOR NOT NEG CMP
        }} {
            my OP $key {*}$args
        } else {
            return -code error [format {unknown method "%s"} $name]
        }
    }
    method CLR args {
        switch [my :model] {
            CM {
                lassign $args a
                my registers: $a 0
            }
            PTM {
                my tape: 0
            }
            SM {
                my stack: 0 0
            }
        }
        dict set data <=> 0
    }
    method DUP args {
        switch [my :model] {
            SM {
                my top
                my push
            }
        }
    }
    method exec {op args} {
        log::log d "[info level 0] / [my extract acc stack]"
        switch $op {
            CMP { dict set data <=> [expr {cmp([my :acc], [my :aux])}] }
            NOT { my acc: [::tcl::mathop::! [my :acc]] }
            NEG { my acc: [::tcl::mathop::* [my :acc] -1] }
            INC { my acc: [expr {[my :acc] + 1}] }
            DEC { my acc: [expr {[my :acc] - 1}] }
            default {
                set op [dict get {
                    EQ eq NE ne EQL == NEQ -!= GT > GE >= LT < LE <=
                    ADD + SUB - MUL * DIV / MOD %
                    AND && OR || XOR ^
                } $op]
                my acc: [::tcl::mathop::$op [my :aux] [my :acc]]
            }
        }
    }
    method OP {op args} {
        switch [my :model] {
            CM {
                lassign $args a b c
                my acc: [my :registers $c]
                my aux: [my :registers $b]
                my exec $op
                my registers: $a [my :acc]
            }
            SM {
                my popx
                my pop
                my exec $op
                my push
            }
        }
    }
    method CPY args {
        switch [my :model] {
            CM {
                lassign $args a b
                my registers: $a [my :registers $b]
            }
        }
    }
    method PUSH args {
        switch [my :model] {
            SM {
                lassign $args a
                if {$a ne {}} {
                    my acc: $a
                }
                my push
            }
        }
    }
    method POP {} {
        switch [my :model] { SM { my pop } }
    }
    method INC args {
        set op INC
        switch [my :model] {
            CM {
                lassign $args a
                my acc: [my :registers $a]
                my exec $op
                my registers: $a [my :acc]
            }
            SM {
                my pop
                my exec $op
                my push
            }
        }
    }
    method DEC args {
        set op DEC
        switch [my :model] {
            CM {
                lassign $args a
                my acc: [my :registers $a]
                my exec $op
                my registers: $a [my :acc]
            }
            SM {
                my pop
                my exec $op
                my push
            }
        }
    }
    method JMP args {
        lassign $args a
        my jmp: $a
    }
    method jmpc {op addr} {
        if {[my :cflag]} {my jmp: $addr}
    }
    method JEQ args {
        my exec EQ {*}$args
        my jmpc eq [lindex $args 0]
    }
    method JNE args {
        my exec EQ {*}$args
        my jmpc [lindex $args 0]
    }
    method JG args {
        my exec GT {*}$args
        my jmpc [lindex $args 0]
    }
    method JGE args {
        my exec GE {*}$args
        my jmpc [lindex $args 0]
    }
    method J0 args {
        my load {*}$args
        if {[my :zflag]} {my jmp: [lindex $args 0]}
    }
    method J1 args {
        my load {*}$args
        if {![my :zflag]} {my jmp: [lindex $args 0]}
    }
    method JZ args {
        my load {*}$args
        if {[my :zflag]} {my jmp: [lindex $args 0]}
    }
    method JNZ args {
        my load {*}$args
        if {![my :zflag]} {my jmp: [lindex $args 0]}
    }

    method CALL args {
        lassign $args a
        my returns: [expr {[my :ipointer] + 1}]
        my jmp: $a
    }
    method RET {} {
        my jmp: [my :returns]
    }
    method NOP args {}
    method HALT args {
        return -code break
    }
    method OUT args {
        lassign $args what act move
        switch $what {
            tape {
                dict with data {
                    set tape [$machine Print $tape $head $act]
                    lassign [$machine Roll $tape $head $move] tape head
                }
                my acc: [my :tape]
            }
            head {
                dict with data {
                    set tape [$machine Print $tape $head $act]
                    lassign [$machine Roll $tape $head [string map {R L L R} $move]] tape head
                }
                my acc: [my :tape]
            }
            robot {
                my robot-act $act
                my robot-move $move
            }
            default { return }
        }
    }
    method TEST args {
        lassign $args a b c
        switch $a {
            front - left - right {
                <get robot facing and position>
                <ask world if f/l/r square is clear>
            }
            next {
                <get robot position>
                <ask world if a beeper is present>
            }
            facing {
                my acc: <get robot facing>
                my aux: [string index $b 0]
                my exec CMP
            }
            any {
                my acc: <get robot #beepers>
                my aux: 0
                my exec GT
            }
            default {
                return -code error [format {unknown test "%s"} $a]
            }
        }
    }

    method merge f {
        set data [dict merge $data $f]
    }

    method step {} {
        dict with data {
            if {$jmp ne {}} {
                set ipointer [
                    if {[regexp {[-+]\d+} $jmp]} {
                        expr $ipointer $jmp
                    } elseif {![string is integer -strict $jmp]} {
                        lindex [$machine matrix search column 0 $jmp] 0 1
                    } else {
                        set jmp
                    }
                ]
                set jmp {}
            } else {
                incr ipointer
            }
        }
    }

    method cycle f {
        log::log d [info level 0] 
        my merge $f
        set ip [my :ipointer]
        while {0 <= $ip && $ip < [$machine matrix rows]} {
            my {*}[lrange [$machine matrix get row $ip] 1 end]
            my step
            set ip [my :ipointer]
        }
    }

    method extract args {
        dict filter $data script {key val} {expr {$key in $args}}
    }
}

