
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

namespace eval automata {}

proc ::tcl::mathfunc::cmp {a b} { if {$a == $b} {return 0} elseif {$a < $b} {return -1} else {return 1}}

oo::class create ::automata::Processor {
    variable data machine

    # TODO eliminate reduntant data items?
    constructor args {
        lassign $args model machine
        my reset $model
    }

    method reset model {
        dict set data model     $model ;# model of computation for this machine
        dict set data acc       0      ;# accumulator
        dict set data aux       0      ;# auxiliary accumulator for binary operations
        dict set data jmp       {}     ;# next address if jumping
        dict set data <=>       0      ;# comparison register (-1, 0, 1)
        dict set data returns   {}     ;# call stack
        dict set data registers {}     ;# registers, for the CM
        dict set data world     {}     ;# world, for the KTR
        dict set data robot     {}     ;# robot, for the KTR
        dict set data tape      {}     ;# tape, for the PTM
        dict set data head      0      ;# tape head, for the PTM
        dict set data stack     {}     ;# stack, for the SM
        dict set data ipointer  0      ;# instruction pointer
    }

    # specialized getter (:foo) and setter (foo:) methods

    method acc: val {
        # Setting acc updates <=> register.
        dict with data {
            set acc $val
            set <=> [expr {cmp($val, 0)}]
        }
    }

    # the flag pseudo-registers depend on <=>
    method :zflag {} { dict with data { expr {${<=>} eq 0} }}

    # make the setter for <=> a no-op
    method <=>: val {}

    # the setter and getter for returns pushes/pops
    method returns: addr {
        dict with data { set returns [linsert $returns 0 $addr] }
    }
    method :returns {} {
        dict with data { set returns [lassign $returns addr] }
        return $addr
    }

    # the setter/getter for registers, used by the CM
    method registers: {idx val} { dict with data { lset registers $idx $val } }
    method :registers {{idx 0}} { dict with data { lindex $registers $idx } }

    # the (indexed) setter/getter for stack, used by the SM
    method stack: {idx val} { dict with data { lset stack $idx $val } }
    method :stack {{idx 0}} { dict with data { lindex $stack $idx } }

    # the setter/getter for tape, used by the PTM
    method tape: val { dict with data { lset tape $head $val } }
    method :tape {} { dict with data { lindex $tape $head } }

    method load {{- {}} {b 0} args} {
        log::log d [info level 0] 
        # load accumulator with a value from the default location
        switch [my :model] {
            CM  { my acc: [my :registers $b] }
            PTM { my acc: [my :tape] }
            SM  { my top }
        }
    }

    method store {{- {}} {b 0} args} {
        log::log d [info level 0] 
        # store accumulator value into the default location
        switch [my :model] {
            CM  { my registers: $b [my :acc] }
            PTM { my tape: [my :acc] }
            SM  { my drop ; my push }
        }
    }

    # stack access (SM only):
    # acc ← stack[0] / acc ← stack[0], drop / aux ← stack[0], drop / push acc / drop 
    method top  {} { dict with data { set top [lindex $stack 0]      } ; my acc: $top }
    method pop  {} { dict with data { set stack [lassign $stack top] } ; my acc: $top }
    method popx {} { dict with data { set stack [lassign $stack top] } ; my aux: $top }
    method push {} { dict with data { set stack [linsert $stack 0 $acc] } }
    method drop {} { dict with data { set stack [lassign $stack top] } }

    # PUSH val: push val onto stack / PUSH: push acc onto stack
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
    export PUSH POP

    # swap values between acc and aux
    method swap {} {
        dict with data {
            set val $aux
            set aux $acc
        }
        my acc: $val
    }
                    
    # clear acc and store in default location
    method CLR args {
        my acc: 0
        my store {} {*}$args
    }

    # copy register a ← b (CM only)
    method CPY args {
        switch [my :model] {
            CM {
                lassign $args a b
                my registers: $a [my :registers $b]
            }
        }
    }

    # duplicate top item (SM only)
    method DUP args {
        switch [my :model] {
            SM {
                my top
                my push
            }
        }
    }

    export CLR CPY DUP

    # arithmetic / logic operations (CM and SM only)
    forward NOT my UNOP !
    forward NEG my UNOP NEG
    forward INC my UNOP INC
    forward DEC my UNOP DEC
    export NOT NEG INC DEC
    method UNOP {op args} {
        my load {} {*}$args
        switch $op {
            NEG { my acc: [::tcl::mathop::* [my :acc] -1] }
            INC { my acc: [expr {[my :acc] + 1}] }
            DEC { my acc: [expr {[my :acc] - 1}] }
            default {
                my acc: [::tcl::mathop::$op [my :acc]]
            }
        }
        my store {} {*}$args
    }
    forward EQ  my BINOP eq
    forward NE  my BINOP ne
    forward EQL my BINOP ==
    forward NEQ my BINOP !=
    forward GT  my BINOP >
    forward GE  my BINOP >=
    forward LT  my BINOP <
    forward LE  my BINOP <=
    forward ADD my BINOP +
    forward SUB my BINOP -
    forward MUL my BINOP *
    forward DIV my BINOP /
    forward MOD my BINOP %
    forward AND my BINOP &&
    forward OR  my BINOP ||
    forward XOR my BINOP ^
    export EQ NE EQL NEQ GT GE LT LE ADD SUB MUL DIV MOD AND OR XOR 
    method BINOP {op args} {
        switch [my :model] {
            CM {
                lassign $args a b c
                my acc: [my :registers $c]
                my aux: [my :registers $b]
            }
            SM {
                my popx
                my pop
            }
        }
        switch $op {
            default {
                my acc: [::tcl::mathop::$op [my :aux] [my :acc]]
            }
        }
        switch [my :model] {
            CM {
                my registers: $a [my :acc]
            }
            SM {
                my push
            }
        }
    }

    # jumps
    method JMP args {
        lassign $args a
        my jmp: $a
    }

    method cmp args {
        lassign $args b c
        switch [my :model] {
            CM { my acc: [expr {cmp([my :registers $c], [my :registers $b])}] }
            SM { my acc: [expr {cmp([my :stack 1], [my :stack 0])}] }
        }
    }
    method JEQ args {
        my cmp {*}[lassign $args addr]
        if {[my :<=>] eq 0} {my jmp: $addr}
    }
    method JNE args {
        my cmp {*}[lassign $args addr]
        if {[my :<=>] ne 0} {my jmp: $addr}
    }
    method JG args {
        my cmp {*}[lassign $args addr]
        if {[my :<=>] > 0} {my jmp: $addr}
    }
    method JGE args {
        my cmp {*}[lassign $args addr]
        if {[my :<=>] >= 0} {my jmp: $addr}
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
    export JMP JEQ JNE JG JGE J0 J1 JZ JNZ

    # subroutine call/return
    method CALL args {
        lassign $args a
        my returns: [expr {[my :ipointer] + 1}]
        my jmp: $a
    }
    method RET args {
        my jmp: [my :returns]
    }
    export CALL RET

    method NOP args {}
    export NOP

    method HALT args {
        return -code break
    }
    export HALT

    # manipulate the machine's environment somehow
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
                switch $act {
                    TURN { my robot: [$machine Turn [my :robot]] }
                    MOVE {
                        my robot: [$machine Move [my :robot]]
                        if {[$machine CheckCollision [my :world] [my :robot]]} {
                            return -code error [format {collision with a wall!}]
                        }
                    }
                }
            }
            default { return }
        }
    }
    export OUT

    # inspect the machine's environment
    method TEST args {
        lassign $args a b c
        switch $a {
            front - left - right {
                my acc: [$machine TestBlocked $a [my :world] [my :robot]]
            }
            next {
                lassign [my :robot] xpos ypos
                my acc: [expr {[list $xpos $ypos] in [lmap {x y} [lindex [my :world] 2] {list $x $y}]}]
            }
            facing {
                my acc: [expr {[lindex [my :robot] 3] eq [string index $b 0]}]
            }
            any {
                my acc: [expr {[lindex [my :robot] 2] > 0}]
            }
            default {
                return -code error [format {unknown test "%s"} $a]
            }
        }
    }
    export TEST

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
        my merge $f
        set ip [my :ipointer]
        while {0 <= $ip && $ip < [$machine matrix rows]} {
            my {*}[lrange [$machine matrix get row $ip] 1 end]
            my step
            set ip [my :ipointer]
        }
    }

    method extract args {
        foreach arg $args {
            switch $arg {
                zflag {
                    dict set res $arg [my :zflag]
                }
                default {
                    dict set res $arg [dict get $data $arg]
                }
            }
        }
        return $res
    }

    # resolve unknown methods
    method unknown {name args} {
        set key [string trim $name :]
        if {$key in [dict keys $data]} {
            switch $name $key: {
                dict set data $key {*}$args
            } :$key {
                dict get $data $key
            }
        } else {
            return -code error [format {unknown method "%s"} $name]
        }
    }
}
