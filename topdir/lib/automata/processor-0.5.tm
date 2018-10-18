
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

namespace eval automata {}

proc ::tcl::mathfunc::cmp {a b} { if {$a == $b} {return 0} elseif {$a < $b} {return -1} else {return 1}}

oo::class create ::automata::Processor {
    variable data matrix
    constructor args {
        lassign $args matrix
        oo::objdefine [self] forward matrix $matrix
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
    }
    method acc: val {
        log::log d [info level 0] 
        dict set data acc $val
        dict set data <=> [expr {cmp($val, 0)}]
    }
    if no {
        method :jmp {} {
            dict with data {
                if {$jmp eq {}} {
                    set addr $ipointer
                } else {
                    set addr $jmp
                    set jmp {}
                }
            }
            return $addr
        }
        export :jmp
    }
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
    method :stack {} { lindex [dict get $data stack] }
    export :stack
    method tape: val { dict with data { lset tape $head $val } }
    method :tape {} { dict with data { lindex tape $head } }
    export :tape
    method pop {} { dict with data { set stack [lassign $stack top] } ; my acc: $top }
    method push {} { dict with data { set stack [linsert $stack 0 $acc] } }
    method xchg {} {
        dict with data {}
        set val $acc
        set acc $aux
        set aux $val
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
                EQ EQL ADD SUB MUL DIV MOD
                AND OR XOR NOT IDENT CMP
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
            CM {
                dict with data {}
                set val [lindex $stack $a]
                set stack [linsert $stack 0 $val]
                dict set data <=> [expr {cmp($val, 0)}]
            }
        }
    }
    method exec {op args} {
        switch $op {
            IDENT { dict set data <=> [expr {cmp([my :acc], [my :acc])}] }
            CMP { dict set data <=> [expr {cmp([my :acc], [my :aux])}] }
            NOT { my acc: [::tcl::mathop::! [my :acc]] }
            INC { my acc: [expr {[my :acc] + 1}] }
            DEC { my acc: [expr {[my :acc] - 1}] }
            default {
                set op [dict get {
                    EQ eq EQL == GT >
                    ADD + SUB - MUL * DIV / MOD %
                    AND && OR || XOR ^ NOT !
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
                my pop
                my xchg
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
    method JE args {
        switch [my :model] {
            CM {
                lassign $args a b c
                my acc: [my :registers $c]
                my aux: [my :registers $b]
                my exec CMP
                if {![my :<=>]} {my jmp: $a}
            }
        }
    }
    method JNE args {
        switch [my :model] {
            CM {
                lassign $args a b c
                my acc: [my :registers $c]
                my aux: [my :registers $b]
                my exec CMP
                if {[my :<=>]} {my jmp: $a}
            }
        }
    }
    method JZ args {
        lassign $args a b c
        switch [my :model] {
            CM {
                my acc: [my :registers $c]
                my aux: 0
            }
            SM {
                my acc: [my :stack 0]
                my aux: 0
            }
            default { return }
        }
        my exec CMP
        if {![my :<=>]} {my jmp: $a}
    }
    method JNZ args {
        lassign $args a b c
        switch [my :model] {
            CM {
                my acc: [my :registers $c]
                my aux: 0
            }
            SM {
                my acc: [my :stack 0]
                my aux: 0
            }
            default { return }
        }
        my exec CMP
        if {[my :<=>]} {my jmp: $a}
    }
    method CALL args {
        lassign $args a
        my returns: [my :ipointer]
        my jmp: $a
    }
    method RET {} {
        my jmp: [my :returns]
    }
    method NOP args {}
    method HALT {} {
        <somehow halt processor>
    }
    method OUT args {
        lassign $args what act move
        switch $what {
            tape {
                my tape-act $act
                my tape-move $move
            }
            head {
                my head-act $act
                my head-move $move
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

    method merge args {
        set data [dict merge $data $args]
    }

    method step {} {
        dict with data {
            if {$jmp ne {}} {
                set ipointer $jmp
            }
        }
    }

    method cycle args {
        my merge $args
        dict with data {
            set oper [lassign [my matrix get row $ipointer] -]
            incr ipointer
            set jmp {}
        }
        my {*}$oper
        my step
    }

    method extract args {
        dict filter $data script {key val} {expr {$key in $args}}
    }
}

