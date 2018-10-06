::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::machine
package require automata::configuration

namespace eval automata {}

# A CodeMachine is an automaton that compiles a labeled set of instructions to
# a transition table.
oo::class create ::automata::CodeMachine {
    mixin ::automata::Configuration ::automata::Machine

    variable types table iddef

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
        dict for {addr data} $program {
            dict with data {
                lassign $lbl a b c d e f
                set next [expr {$addr + 1}]
                if {[regexp {^[-+]\d+$} $a]} {
                    set a [expr $addr$a]
                } elseif {[dict exists $jumps $a]} {
                    set a [dict get $jumps $a]
                }
                # Conditional jumps:
                # JN.* jump if not cond(flag)
                # J.*  jump if (flag)
                #
                # Flag setting:
                # CM:  reg' eq reg''
                # KTR: 0 or as set by TEST:
                # PTM: tape/head
                # SM:
                #   JN?SZ: stack#0 eq zero
                #   JN?SE: stack#0 eq stack#1
                #   else:  0
                switch -glob $cmd {
                    J: - CALL: {
                        set addresses [list $a $a]
                    }
                    JN* {
                        set addresses [list $a $next]
                    }
                    J* {
                        set addresses [list $next $a]
                    }
                    default {
                        set addresses [list $next $next]
                    }
                }
                switch $cmd {
                    JZ: - JNZ: {
                        set code [list [string map {N {}} $cmd] $b 0]
                    }
                    JE: - JNE: {
                        set code [list [string map {N {}} $cmd] $b $c]
                    }
                    JSZ: - JNSZ: - JSE: - JNSE: {
                        set code [string map {N {}} $cmd]
                    }
                    J: {
                        set code NOP
                    }
                    CALL: {
                        set code $cmd
                    }
                    TEST: {
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
                foreach inp [$types get A] next $addresses {
                    $table add $addr $inp $next $code
                }
            }
        }
    }

    method ALU {op data args} {
        # Shared between KTR and SM
        switch $op {
            INC { set res [expr {[lindex $data {*}$args] + 1}] }
            DEC { set res [expr {[lindex $data {*}$args] - 1}] }
            CLR { set res 0 }
            default {
                if {[string is upper -strict $op]} {
                    set op [dict get {
                        EQ  eq
                        EQL ==
                        ADD +
                        MUL *
                    } $op]
                }
                set res [::tcl::mathop::$op {*}[lmap arg $args {
                    lindex $data $arg
                }]]
            }
        }
        if {$res < 0} {
            return -code error [format {result less than 0}]
        }
        return $res
    }

    method SingleThread {fn data} {
        set data [list {*}$data]
        set id [$iddef make $data [$types get S]]
        set results [my search $id $fn]
        dict values [lindex $results 0]
    }
}

oo::class create ::automata::CM {
    mixin ::automata::CodeMachine

    variable types table iddef

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
        }
        my installOperations $instructionSet
        my installRunMethod {
            registers {} {a list of initial register cells}
        }
        my type A "Flag symbols"    {@ 0 1}
        my type V "Register values" N -hidden 1
        my type I "Instructions"    [linsert $instructionSet 0 @] -hidden 1
        my type Q "Addresses"       N+
        my type S "Start address"   Q
        my type F "Final address"   Q
        my type E "Erase symbol"    {@ 0}
        my type O "Operations"      #+ -hidden 1
        my table Q A Q O*
        my id {
            registers "register cells"      V*
            ipointer  "instruction pointer" Q 
        }

    }

    method Exec id {
        # unpack ID
        dict with id {
            if {[$types in F $ipointer]} {
                return
            }
            # get move
            lassign [lindex [$table get $ipointer 0] 0] - - - code
            lassign $code tag a b
            set flag [expr {[lindex $registers $a] eq [lindex $registers $b]}]
            lassign [lindex [$table get $ipointer $flag] 0] - - next
            # build new ID
            switch $tag {
                INC: { lset registers $a [expr {[lindex $registers $a] + 1}] }
                DEC: { lset registers $a [expr {[lindex $registers $a] - 1}] }
                CLR: { lset registers $a [lindex $registers 0] }
                CPY: { lset registers $a [lindex $registers $b] }
                NOP  {}
            }
            if {[lindex $registers 0] ne 0} {
                return -code error [format {registers 0 has been changed}]
            }
            set res [list [$iddef make $registers $next]]
        }
        return $res
    }

    forward Run my SingleThread Exec

}

oo::class create ::automata::KTR {
    mixin ::automata::CodeMachine

    variable types table iddef
    
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
        }
        my type A "Flag symbols"    {@ 0 1}
        my type B "Facing"          {@ 0 1 2 3} -hidden 1
        my type I "Instructions"    {@ JZ: J: TURN MOVE TAKE DROP TEST: RET CALL:} -hidden 1
        my type Q "Addresses"       N+
        my type S "Start address"   Q
        my type F "Final address"   Q
        my type V "Values"          N -hidden 1
        my table Q A Q O*
        my id {
            width    "world width"         V 
            height   "world height"        V 
            xpos     "robot x"             V 
            ypos     "robot y"             V 
            bag      "#robot's beepers"    V 
            facing   "robot facing"        B 
            returns  "return stack"        Q*
            flag     "test flag"           A 
            beepers  "beeper coords"       V*
            walls    "wall coords"         V*
            ipointer "instruction pointer" Q 
        }
        my type O "Operations"      #+ -hidden 1
    }

    method Turn {varName {a 1}} {
        upvar 1 $varName facing
        set facing [expr {($facing + $a + 4) % 4}]
    }

    method RMove {width height varName1 varName2 facing walls} {
        upvar 1 $varName1 xpos $varName2 ypos
        switch $facing {
            0 { set xpos [expr {$xpos + 1}] }
            1 { set ypos [expr {$ypos + 1}] }
            2 { set xpos [expr {$xpos - 1}] }
            3 { set ypos [expr {$ypos - 1}] }
        }
        if {[my CheckCollision $width $height $xpos $ypos $walls]} {
            return -code error [format {collision with a wall!}]
        }
    }

    method Look {xpos ypos facing {ddir 0}} {
        switch [expr {($facing + $ddir + 4) % 4}] {
            0 { incr xpos }
            1 { incr ypos }
            2 { incr xpos -1 }
            3 { incr ypos -1 }
        }
        return [list $xpos $ypos]
    }

    method FindBeeper {xpos ypos beepers} {
        foreach {X Y} $beepers {
            if {$X eq $xpos && $Y eq $ypos} {
                return 1
            }
        }
        return 0
    }

    method CheckCollision {width height xpos ypos walls} {
        set _walls [list 0 $ypos [expr {$width + 1}] $ypos $xpos 0 $xpos [expr {$height + 1}]]
        foreach {X Y} [concat $_walls $walls] {
            if {$X eq $xpos && $Y eq $ypos} {return 1}
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
                    expr {![my CheckCollision $width $height {*}[my Look $xpos $ypos $facing] $walls]}
                }
                left-is-clear {
                    expr {![my CheckCollision $width $height {*}[my Look $xpos $ypos $facing +1] $walls]}
                }
                right-is-clear {
                    expr {![my CheckCollision $width $height {*}[my Look $xpos $ypos $facing -1] $walls]}
                }
                next-to-a-beeper { my FindBeeper $xpos $ypos $beepers }
                facing-east  { expr {$facing eq 0} }
                facing-north { expr {$facing eq 1} }
                facing-west  { expr {$facing eq 2} }
                facing-south { expr {$facing eq 3} }
                any-beepers-in-beeper-bag { expr {$bag > 0} }
            }
        }
    }

    method Exec id {
        log::log d [info level 0] 
        # unpack ID
        dict with id {
            # get move
            set next [$types succ Q $ipointer]
            lassign [lindex [$table get $ipointer 0] 0] - - - code
            lassign $code tag _a _b _c _d _e
            lassign [lindex [$table get $ipointer $flag] 0] - - i
            set flag 0
            switch $tag {
                HALT  {
                    return
                }
                TURN  {
                    my Turn facing
                }
                MOVE  {
                    my RMove $width $height xpos ypos $facing $walls
                }
                TAKE - DROP {
                }
                TEST: {
                    set flag [my Test $id $_a]
                }
                RET   {
                    set returns [lassign $returns i]
                }
                CALL: {
                    set returns [linsert $returns 0 $next]
                }
                NOP {}
            }
        }
        if {[$types in F $i]} {
            return
        }
        # build new ID
        lappend _id $width $height
        lappend _id $xpos $ypos $bag $facing
        lappend _id $returns
        lappend _id $flag
        lappend _id $beepers
        lappend _id $walls
        lappend _id $i
        return [list [$iddef make {*}$_id]]
    }

    method Run {world robot beepers walls} {
        #: Run the code with the given configuration, starting from s.
        lappend id {*}$world
        lappend id {*}$robot
        lappend id [list]
        lappend id 0
        lappend id $beepers
        lappend id $walls
        lappend id [$types get S]
        set id [$iddef make {*}$id]
        set results [my search $id Exec]
        set res [dict values [lindex $results 0]]
        list [lrange $res 0 1] [lrange $res 2 5] [lindex $res 6] [lindex $res 7] [lindex $res 8] [lindex $res 9] [lindex $res 10]
    }

}

oo::class create ::automata::PTM {
    mixin ::automata::CodeMachine

    variable types table iddef

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
        }
        my type A "Tape symbols"    {@ 0 1}
        my type B "Move symbols"    {@ L R} -hidden 1
        my type I "Instructions"    {@ JZ: J: PRINT ERASE ROLL:} -hidden 1
        my type Q "Addresses"       N+
        my type S "Start address"   Q
        my type F "Final address"   Q
        my type O "Operations"      #+ -hidden 1
        my type V "Values"          N -hidden 1
        my table Q A Q O*
        my id {
            tape     "tape contents"       A*
            head     "current index"       V 
            ipointer "instruction pointer" Q 
        }
    }

    # Print and Move are borrowed from the BTM

    method Exec id {
        # unpack ID
        dict with id {
            if {[$types in F $ipointer]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [$table get $ipointer [lindex $tape $head]]
            if {[llength $tuples] eq 0} {
                return
            }
            set tuple [lindex $tuples 0]
            lassign $tuple - - i code
            lassign $code tag a b
            switch $tag {
                HALT  {
                    return
                }
                PRINT: {
                    lset tape $head [lindex [$types get A] $a]
                }
                ROLL: {
                    # PTM has reversed sense of movement
                    my Move tape head [string map {R L L R} $a]
                }
                NOP {}
            }
            set ids [list [$iddef make $tape $head $i]]
        }
        return $ids
    }

    method Run tape {
        #: Run the code on this tape, return tape.
        set tape [list {*}$tape]
        set ids [lmap ipointer [$types get S] {
            $iddef make $tape 0 $ipointer
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
    mixin ::automata::CodeMachine

    variable types table iddef

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
        }
        my type A "Flag symbols"    {@ 0 1}
        my type I "Instructions"    {@ JZ: JSZ: JSE: J: PUSH INC DEC CLR DUP EQ EQL ADD MUL} -hidden 1
        my type Q "Addresses"       N+
        my type S "Start address"   Q
        my type F "Final address"   Q
        my type O "Operations"      #+ -hidden 1
        my type V "Values"          N -hidden 1
        my table Q A Q O*
        my id {
            stack    "stack contents"      V*
            ipointer "instruction pointer" Q 
        }
    }

    method Exec id {
        # unpack ID
        dict with id {
            if {[$types in F $ipointer]} {
                return
            }
            lassign [lindex [$table get $ipointer 0] 0] - - - code
            lassign $code tag a b c d e f
            lassign $stack TOP
            switch $tag {
                JSZ: {
                    set flag [expr {$TOP == 0}]
                }
                JSE: {
                    set flag [expr {$TOP == [lindex $stack 1]}]
                }
                default {
                    set flag 0
                }
            }
            lassign [lindex [$table get $ipointer $flag] 0] - - i
            # get move
            switch $tag {
                PUSH {
                    set stack [linsert $stack 0 $a]
                }
                INC - DEC - CLR {
                    lset stack 0 [my ALU $tag $stack 0]
                }
                DUP {
                    set stack [linsert $stack 0 $TOP]
                }
                EQ - EQL - ADD - MUL {
                    set v [my ALU $tag $stack 0 1]
                    set stack [lreplace $stack 0 1 $v]
                }
            }
            if {[lindex $stack 0] < 0} {
                return -code error [format {negative value in top of stack}]
            }
            # build new ID
            list [$iddef make $stack $i]
        }
    }

    forward Run my SingleThread Exec

}
