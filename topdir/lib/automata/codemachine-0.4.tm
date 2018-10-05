::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::machine
package require automata::configuration

namespace eval automata {}

# A CodeMachine is an automaton that compiles a labeled set of instructions to
# a transition table.
oo::class create ::automata::CodeMachine {

    variable types table

    method compile tokens {
        #: Convert source code to transition configuration.
        #:
        foreach token $tokens {
            my AddOp $token
        }
        my GetOps
    }

    method AddOp token {
        variable addr
        variable program
        variable jumps
        if {![info exists addr]} {
            set addr 0
        }
        if {![info exists jumps]} {
            dict set jumps BEG_OF_CODE $addr
        }
        if {![regexp {(\w+:?)(.*)} $token -> cmd lbl]} {
            return -code error [format {syntax error: %s} $token]
        }
        set lblargs [regexp -all -inline {[-+\w]+} $lbl]
        if {[string match *: $cmd] && [llength $lblargs] eq 0} {
            dict set jumps [string trimright $cmd :] $addr
        } else {
            dict set program $addr cmd $cmd
            dict set program $addr lbl $lblargs
            dict set jumps END_OF_CODE [incr addr]
        }
    }

    method GetOps {} {
        variable program
        variable jumps
        $types set S [dict get $jumps BEG_OF_CODE]
        $types set F [dict get $jumps END_OF_CODE]
        my add S [dict get $jumps BEG_OF_CODE]
        my add F [dict get $jumps END_OF_CODE]
        dict for {addr data} $program {
            dict with data {
                lassign $lbl a b c d e f
                set next [expr {$addr + 1}]
                if {[regexp {^[-+]\d+$} $a]} {
                    set a [expr $addr$a]
                } elseif {[dict exists $jumps $a]} {
                    set a [dict get $jumps $a]
                }
                set addresses [list $next $next]
                # TODO planned:
                # test flag:
                #   CM:  condition registers same (implied EQ)
                #   KTR: test operation
                #   PTM: current cell (implied READ)
                #   SM:  top of stack (implied TOP)
                # J(N)Z:  check if test flag is zero/non-zero
                # J0:     check if test flag is 0
                # J1:     check if test flag is 1
                # J(N)T:  check if test flag is true/false
                # JF:     check if test flag is false
                # JS(N)E: check if test flag same as second stack item
                switch $cmd {
                    JE: - JZ: - JSE: - JSZ: {
                        # JE has two valid args,
                        # JZ one,
                        # JSE and JSZ zero
                        set addresses [list $next $a]
                        set code [list $cmd $b $c]
                    }
                    JNE: - JNZ: - JSNE - JSNZ: {
                        set addresses [list $a $next]
                        set code [string map {N {}} $cmd] $b $c]
                    }
                    JT: {
                        set addresses [list $next $a]
                        set code NOP
                    }
                    JNT: {
                        set addresses [list $a $next]
                        set code NOP
                    }
                    J: {
                        set addresses [list $a $a]
                        set code NOP
                    }
                    CALL: {
                        set addresses [list $a $a]
                        set code $cmd
                    }
                    TEST: {
                        set addresses [list $next $next]
                        set code [list $cmd [lsearch -exact {
                            front-is-clear
                            left-is-clear
                            right-is-clear
                            next-to-a-beeper
                            facing-north
                            facing-south
                            facing-east
                            facing-west
                            any-beepers-in-beeper-bag
                        } $lbl]]
                    }
                    PRINT {
                        set code [list PRINT: 1]
                    }
                    ERASE {
                        set code [list PRINT: 0]
                    }
                    ROLL: {
                        set code [list ROLL: $a]
                    }
                    INC - DEC - EQ - EQL - ADD - MUL {
                        set code $cmd
                    }
                    INC: - DEC: - CLR: {
                        set code [list $cmd $a]
                    }
                    CPY: {
                        set code [list $cmd $a $b]
                    }
                    default {
                        if {[string is entier -strict $cmd]} {
                            set addresses [list $next $next]
                            set code [list PUSH $cmd]
                        } else {
                            set addresses [list $next $next]
                            set code $cmd
                        }
                    }
                }
                set code [lmap c $code {
                    if {$c eq {}} {
                        continue
                    } else {
                        set c
                    }
                }]
                foreach inp [my get A] next $addresses {
                    $table add $addr $inp $next $code
                    my add table $addr $inp $next $code
                }
            }
        }
    }

    method SingleThread {fn data} {
        set data [list {*}$data]
        set id [my AddID $data [my get S]]
        set results [my search $id $fn]
        dict values [lindex $results 0]
    }
}

oo::class create ::automata::CM {
    mixin ::automata::CodeMachine ::automata::Configuration ::automata::Machine

    variable table iddef

    constructor args {
        set is 4
        if {[lindex $args 0] eq "-instructionset"} {
            set args [lassign $args - is]
        }
        set instructionSet [dict get {
            1 {INC: DEC: JZ:}
            2 {CLR: INC: JE:}
            3 {INC: CPY: JE:}
            4 {INC: DEC: CLR: CPY: J: JZ:}
        } $is]
        my add doc preamble {
A Counter Machine is the simplest form of Register Machine.

The number of registers is determined by the length of the list passed to the `run` method. Registers are indexed in the same way as a list index (0, 1, 2, ...).

Register #0 is reserved for the value 0. It is an error to set it to another value.

Specify which actual instruction set to use when instantiating machine.

* `-instructionset 1` : (INC, DEC, JZ), (Minsky (1961, 1967), Lambek (1961))
* `-instructionset 2` : (CLR, INC, JE), (Ershov (1958), Peter (1958) as interpreted by Shepherdson-Sturgis (1964); Minsky (1967); Schönhage (1980))
* `-instructionset 3` : (INC, CPY, JE), (Elgot-Robinson (1964), Minsky (1967))
* `-instructionset 4` : (INC, DEC, CLR, CPY, J, JZ) (default: Shepherdson and Sturgis (1963))
        }
        if no {
            runargs {registers "a list of register cells"}
            table Q A Q #*
            id {
            }
        }
        my installOperations $instructionSet
        my installRunMethod {
            registers {} {a list of initial register cells}
        }
        my type A "Flag symbols"    {@ 0 1}
        my type B "Register values" N -hidden 1
        my type Q "Instructions"    N+
        my type E "Erase symbol"    {@ 0}
        my type S "Program start"   Q
        my type F "Program end"     Q
        my type O "Operations"      #+ -hidden 1
        my table1 Q A Q O*
        my id1 {
            register "register cells"      N*
            ipointer "instruction pointer" Q 
        }
        my graded "Flag symbols"    A -domain B
        my graded "Register values" B -domain N -hide
        my graded "Instructions"    Q -domain N
        my graded "Erase symbol"    E -scalar -default 0
        my graded "Program start"   S -scalar
        my graded "Program end"     F
        my graded "Operations"      O -hide
        my table -as {Q A Q O*}
        my id {
            r B* "registers"
            i Q  "instruction pointer"
        }

    }

    method Exec id {
        # unpack ID
        dict with id {
            if {[my in F $i]} {
                return
            }
            # get move
            lassign [lindex [my get table $i 0] 0] - - - code
            lassign $code tag a b
            switch $tag {
                JE: { set flag [expr [lindex $r $a] eq [lindex $r $b]] }
                JZ: { set flag [expr [lindex $r $a] eq [lindex $r 0]] }
                default {
                    set flag 0
                }
            }
            lassign [lindex [my get table $i $flag] 0] - - i
            # build new ID
            switch $tag {
                INC: { lset r $a [expr {[lindex $r $a] + 1}] }
                DEC: { lset r $a [expr {[lindex $r $a] - 1}] }
                CLR: { lset r $a [lindex $r 0] }
                CPY: { lset r $a [lindex $r $b] }
                NOP  {}
            }
            if {[lindex $r 0] ne 0} {
                return -code error [format {register 0 has been changed}]
            }
            set res [list [my add id $r $i]]
        }
        return $res
    }

    forward Run my SingleThread Exec

}

oo::class create ::automata::KTR {
    mixin ::automata::CodeMachine ::automata::Configuration ::automata::Machine

    variable table iddef
    
    constructor args {
        my add doc preamble {
This is a very limited Karel the Robot that can only walk around, not interact with beepers.

Test numbers:

| Number | Test                      |
| :---:  | :---                      |
| 0    | front-is-clear    |
| 1    | left-is-clear     |
| 2    | right-is-clear    |
| 3    | next-to-a-beeper     |
| 4  | facing-east          |
| 5   | facing-north         |
| 6  | facing-west          |
| 7  | facing-south         |
| 8     | any-beepers-in-beeper |
        }
        my installOperations {JT: JNT: J: HALT TURN MOVE TAKE DROP TEST: RET CALL:}
        my installRunMethod {
            world   {} {a list of width, height values (integer)}
            robot   {} {a list of x, y, n, f values (integer)}
            beepers {} {an even-sized list of x, y values}
            walls   {} {an even-sized list of x, y values}
            ?start? {} {initial state}
        }
        if no {
            runargs {
                world   "a list of width, height values"
                robot   "a list of x, y, n, f values"
                beepers "an even-sized list of x, y values"
                walls   "an even-sized list of x, y values"
            }
            table Q A Q #*
        }
        my type A "Flag symbols"    {@ 0 1}
        my type B "Facing"          {@ 0 1 2 3} -hidden 1
        my type I "Instructions"    {@ JZ: J: TURN MOVE TAKE DROP TEST: RET CALL:} -hidden 1
        my type Q "Addresses"       N+
        my type S "Start address"   Q
        my type F "Final address"   Q
        my table1 Q A Q O*
        my id1 {
            world    "world width"         N 
            height   "world height"        N 
            xpos     "robot x"             N 
            ypos     "robot y"             N 
            bag      "#robot's beepers"    N 
            facing   "robot facing"        B 
            returns  "return stack"        Q*
            test     "test state"          A 
            beepers  "beeper coords"       N*
            walls    "wall coords"         N*
            ipointer "instruction pointer" Q 
        }
        my type O "Operations"      #+ -hidden 1
        my graded "Flag symbols"    A -domain B
        my graded "Lengths/Amounts" B -domain N -hide
        my graded "Facing"          C -enum {0 1 2 3} -hide
        my graded "Instructions"    Q -domain N
        my graded "Program start"   S -scalar
        my graded "Program end"     F
        my graded "Operations"      O -hide
        my table -as {Q A Q O*}
        my id {
            w B  "world width"
            h B  "world height"
            x B  "robot x"
            y B  "robot y"
            n B  "robot's beepers"
            f C  "robot facing"
            r Q* "return stack"
            t A  "test state"
            b B* "beeper coords"
            a B* "wall coords"
            i Q  "instruction pointer"
        }
    }

    method Turn {varName {a 1}} {
        upvar 1 $varName f
        set f [expr {($f + $a + 4) % 4}]
    }

    method RMove {w h varName1 varName2 f a} {
        upvar 1 $varName1 x $varName2 y
        switch $f {
            0 { set x [expr {$x + 1}] }
            1 { set y [expr {$y + 1}] }
            2 { set x [expr {$x - 1}] }
            3 { set y [expr {$y - 1}] }
        }
        if {[my CheckCollision $w $h $x $y $a]} {
            return -code error [format {collision with a wall!}]
        }
    }

    method Look {x y f {ddir 0}} {
        switch [expr {($f + $ddir + 4) % 4}] {
            0 { incr x }
            1 { incr y }
            2 { incr x -1 }
            3 { incr y -1 }
        }
        return [list $x $y]
    }

    method FindBeeper {x y b} {
        foreach {X Y} $b {
            if {$X eq $x && $Y eq $y} {
                return 1
            }
        }
        return 0
    }

    method CheckCollision {w h x y a} {
        set _a [list 0 $y [expr {$w + 1}] $y $x 0 $x [expr {$h + 1}]]
        foreach {X Y} [concat $_a $a] {
            if {$X eq $x && $Y eq $y} {return 1}
        }
        return 0
    }

    method Test {id idx} {
        dict with id {
            set label [lindex {
                front-is-clear
                left-is-clear
                right-is-clear
                next-to-a-beeper
                facing-north
                facing-south
                facing-east
                facing-west
                any-beepers-in-beeper-bag
            } $idx]
            switch $label {
                front-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f] $a]}
                }
                left-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f +1] $a]}
                }
                right-is-clear {
                    expr {![my CheckCollision $w $h {*}[my Look $x $y $f -1] $a]}
                }
                next-to-a-beeper { my FindBeeper $x $y $b }
                facing-east  { expr {$f eq 0} }
                facing-north { expr {$f eq 1} }
                facing-west  { expr {$f eq 2} }
                facing-south { expr {$f eq 3} }
                any-beepers-in-beeper-bag { expr {$n > 0} }
            }
        }
    }

    method Exec id {
        log::log d [info level 0] 
        # unpack ID
        dict with id {
            # get move
            set next [my succ Q $i]
            lassign [lindex [my get table $i 0] 0] - - - code
            lassign $code tag _a _b _c _d _e
            # test-sensitive jumps are coded as NOP
            if {$tag eq "NOP"} {
                set flag $t
            } else {
                set flag 0
            }
            set t 0
            lassign [lindex [my get table $i $flag] 0] - - i
            switch $tag {
                JT: - NOP {}
                HALT  {
                    return
                }
                TURN  {
                    my Turn f
                }
                MOVE  {
                    my RMove $w $h x y $f $a
                }
                TAKE - DROP {
                }
                TEST: {
                    set t [my Test $id $_a]
                }
                RET   {
                    set r [lassign $r i]
                }
                CALL: {
                    set r [linsert $r 0 $next]
                    set t 0
                }
            }
        }
        if {[my in F $i]} {
            return
        }
        # build new ID
        lappend _id $w $h
        lappend _id $x $y $n $f
        lappend _id $r
        lappend _id $t
        lappend _id $b
        lappend _id $a
        lappend _id $i
        return [list [my add id {*}$_id]]
    }

    method Run {world robot beepers walls} {
        #: Run the code with the given configuration, starting from s.
        lappend id {*}$world
        lappend id {*}$robot
        lappend id [list]
        lappend id 0
        lappend id $beepers
        lappend id $walls
        lappend id [my get S]
        set id [my add id {*}$id]
        set results [my search $id Exec]
        set res [dict values [lindex $results 0]]
        list [lrange $res 0 1] [lrange $res 2 5] [lindex $res 6] [lindex $res 7] [lindex $res 8] [lindex $res 9] [lindex $res 10]
    }

}

oo::class create ::automata::PTM {
    mixin ::automata::CodeMachine ::automata::Configuration ::automata::Machine

    variable table iddef

    constructor args {
        my add doc preamble {
A Post-Turing Machine is essentially a TM. The transition matrix
is set by compiling a program.  The tape uses a binary symbol set
(here, 0, 1).
        }
        my installOperations {JZ: JNZ: J: PRINT ERASE ROLL: NOP HALT}
        my installRunMethod {
            tape {} {a list of initial tape symbols}
            ?head? {} {initial head position}
        }
        if no {
            runargs {tape "a (part of a) list of tape symbols"}
            table Q A Q #*
            id {
                tape     A* "tape contents"
                head     N  "current index"
                ipointer Q  "instruction pointer"
            }
        }
        my type A "Tape symbols"    {@ 0 1}
        my type B "Move symbols"    {@ L R} -hidden 1
        my type I "Instructions"    {@ JZ: J: PRINT ERASE ROLL:} -hidden 1
        my type Q "Addresses"       N+
        my type S "Start address"   Q
        my type F "Final address"   Q
        my type O "Operations"      #+ -hidden 1
        my table1 Q A Q O*
        my id1 {
            tape     "tape contents"       A*
            head     "current index"       N 
            ipointer "instruction pointer" Q 
        }
        my graded "Tape symbols"  A -domain B
        my graded "Instructions"  Q -domain N
        my graded "Program start" S -scalar
        my graded "Program end"   F
        my graded "Head position" H -domain N -default 0 -scalar -hide
        my graded "Operations"    O -hide
        my table -as {Q A Q O*}
        my id {
            t A* "tape"
            h H  "current cell"
            q Q  "current state"
        }
    }

    # Print and Move are borrowed from the BTM

    method Exec id {
        # unpack ID
        dict with id {
            if {[my in F $q]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [my get table $q [lindex $t $h]]
            if {[llength $tuples] eq 0} {
                return
            }
            set tuple [lindex $tuples 0]
            lassign $tuple - - q code
            lassign $code tag a b
            switch $tag {
                HALT  {
                    return
                }
                PRINT: {
                    lset t $h [lindex [my get A] $a]
                }
                ROLL: {
                    # PTM has reversed sense of movement
                    my Move t h [string map {R L L R} $a]
                }
                NOP {}
            }
            set ids [list [my add id $t $h $q]]
        }
        return $ids
    }

    method Run tape {
        #: Run the code on this tape, return tape.
        set tape [list {*}$tape]
        set ids [lmap q [my get S] {
            my AddID $tape [my get H] $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict values $result
        }
    }

}

oo::class create ::automata::SM {
    mixin ::automata::CodeMachine ::automata::Configuration ::automata::Machine

    variable table iddef

    constructor args {
        my add doc preamble {
A simple sort of virtual Stack Machine.
        }
        my installOperations {JSZ: JSNZ: JSE: JSNE: J: PUSH INC DEC CLR DUP EQ EQL ADD MUL}
        my installRunMethod {
            stack {} {a list of initial stack symbols}
        }
        if no {
            runargs {stack "a list of stack symbols"}
            table Q A Q #*
        }
        my type A "Flag symbols"    {@ 0 1}
        my type I "Instructions"    {@ JZ: JS: J: PUSH INC DEC CLR DUP EQ EQL ADD MUL} -hidden 1
        my type Q "Addresses"       N+
        my type S "Start address"   Q
        my type F "Final address"   Q
        my type O "Operations"      #+ -hidden 1
        my table1 Q A Q O*
        my id1 {
            stack    "stack contents"      N*
            ipointer "instruction pointer" Q 
        }
        my graded "Flag symbols"  A -domain B
        my graded "Stack values"  B -domain N -hide
        my graded "Instructions"  Q -domain N
        my graded "Program start" S -scalar
        my graded "End points"    F
        my graded "Operators"     O -hide
        my table -as {Q A Q O*}
        my id {
            s B* "current stack"
            i Q  "instruction pointer"
        }
    }

    method Exec id {
        # unpack ID
        dict with id {
            if {[my in F $i]} {
                return
            }
            lassign [lindex [my get table $i 0] 0] - - - code
            lassign $code tag a b c d e f
            lassign $s TOP
            switch $tag {
                JSZ: {
                    set flag [expr {$TOP == 0}]
                }
                JSE: {
                    set flag [expr {$TOP == [lindex $s 1]}]
                }
                default {
                    set flag 0
                }
            }
            lassign [lindex [my get table $i $flag] 0] - - i
            # get move
            switch $tag {
                PUSH {
                    set s [linsert $s 0 $a]
                }
                INC - DEC - CLR {
                    lset s 0 [my ALU $tag $s 0]
                }
                DUP {
                    set s [linsert $s 0 $TOP]
                }
                EQ - EQL - ADD - MUL {
                    set v [my ALU $tag $s 0 1]
                    set s [lreplace $s 0 1 $v]
                }
            }
            if {[lindex $s 0] < 0} {
                return -code error [format {negative value in top of stack}]
            }
            # build new ID
            list [my add id $s $i]
        }
    }

    forward Run my SingleThread Exec

}
