::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::machine
package require automata::compiler

namespace eval automata {}

# A CodeMachine is an automaton that compiles a labeled set of instructions to
# a transition table.
oo::class create ::automata::CodeMachine {
    mixin ::automata::Machine

    variable table iddef

    constructor args {
        next {*}$args
    }

    method addTable args {
        log::log d [info level 0] 
        $table add {*}$args
    }

    if no {
        method compile tokens {
            ::automata::Compiler compile [self] $tokens
        }
    } else {

    method Compile tokens {
        #: Convert source code to transition configuration.
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
        my vsets set S [dict get $jumps BEG_OF_CODE]
        my vsets set F [dict get $jumps END_OF_CODE]
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
                # CM:  reg/a eq reg/b
                # KTR: 0 or as set by TEST:
                # PTM: tape/head
                # SM:  stack/0 eq 0 (JZ) or stack/a eq stack/b (JE)
                switch -glob $cmd {
                    J: - CALL: {
                        set addresses [list $a $a]
                    }
                    JN* {
                        set addresses [list $a $next]
                        set cmd [string replace $cmd 1 1]
                    }
                    J* {
                        set addresses [list $next $a]
                    }
                    HALT {
                        set end [my vsets get F]
                        set addresses [list $end $end]
                    }
                    default {
                        set addresses [list $next $next]
                    }
                }
                switch $cmd {
                    JZ: {
                        set code [list JC: $b 0]
                    }
                    JE: {
                        set code [list JC: $b $c]
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
                            set code [list PUSH $cmd]
                        } else {
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
                foreach inp [my vsets get A] next $addresses {
                    $table add $addr $inp $next $code
                }
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
        set id [$iddef make $data [my vsets get S]]
        set results [my search $id $fn]
        dict values [lindex $results 0]
    }
}

oo::class create ::automata::CM {
    mixin ::automata::CodeMachine

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
        my runAs run "run the machine" {registers "a list of initial register cells"}
        my values A "Flag symbols"     {@ 0 1}
        my values P "Print directives" N+ -hidden 1
        my values M "Move directives"  {@ L R} -hidden 1
        my values Q "Addresses"        N+
        my values S "Start address"    Q
        my values F "Final address"    Q
        my values E "Erase symbol"     {@ 0}
        my values O "Operations"       #+ -hidden 1
        my values I "Instructions"     [linsert $instructionSet 0 @] -hidden 1
        my values V "Register values"  N -hidden 1
        my table Q A Q P M O*
        my id {
            registers "register cells"      V*
            ipointer  "instruction pointer" Q 
        }
    }

    method compile tokens {
        ::automata::Compiler compile [self] $tokens
    }

    method SetFlag {registers tag args} {
        lassign $args a b
        expr {[lindex $registers $a] eq [lindex $registers $b]}
    }

    method ExecCode {varName code} {
        upvar 1 $varName registers
        lassign $code tag a b
        switch $tag {
            INC: { lset registers $a [expr {[lindex $registers $a] + 1}] }
            DEC: { lset registers $a [expr {[lindex $registers $a] - 1}] }
            CLR: { lset registers $a [lindex $registers 0] }
            CPY: { lset registers $a [lindex $registers $b] }
            NOP  {}
        }
        if {[lindex $registers 0] ne 0} {
            return -code error [format {register 0 has been changed}]
        }
    }

    method ExecCode {varName code} {
        upvar 1 $varName registers
        lassign $code tag a b
        switch $tag {
            I { lset registers $a [expr {[lindex $registers $a] + 1}] }
            D { lset registers $a [expr {[lindex $registers $a] - 1}] }
            CL { lset registers $a [lindex $registers 0] }
            CP { lset registers $a [lindex $registers $b] }
            N  {}
        }
        if {[lindex $registers 0] ne 0} {
            return -code error [format {register 0 has been changed}]
        }
    }

    method Exec id {
        dict with id {
            if {[my vsets in F $ipointer]} {
                return
            }
            set ids [$table map $ipointer {iSym target print move code} {
                set flag [my SetFlag $registers {*}$code]
                if {$iSym eq $flag} {
                    my ExecCode registers $code
                    $iddef make $registers $target
                } else {
                    continue
                }
            }]
        }
        return $ids
    }

    forward Run my SingleThread Exec

}

oo::class create ::automata::KTR {
    mixin ::automata::CodeMachine

    variable table iddef
    
    constructor args {
        my runAs run "run the machine" {
            world   "a list of width, height values (integer)"
            robot   "a list of x, y, n, f values (integer)"
            beepers "an even-sized list of x, y values"
            walls   "an even-sized list of x, y values"
        }
        my values A "Flag symbols"    {@ 0 1}
        my values B "Facing"          {@ e n w s} -hidden 1
        my values Q "Addresses"       N+
        my values S "Start address"   Q
        my values F "Final address"   Q
        my values O "Operations"      #+ -hidden 1
        my values I "Instructions"    {@ JZ: J: TURN MOVE TAKE DROP TEST: RET CALL:} -hidden 1
        my values V "Values"          N -hidden 1
        my table Q A Q O*
        my id {
            width    "world width"         V 
            height   "world height"        V 
            xpos     "robot x"             V 
            ypos     "robot y"             V 
            bag      "#robot's beepers"    V 
            facing   "robot facing"        B 
            returns  "return stack"        Q*
            beepers  "beeper coords"       V*
            walls    "wall coords"         V*
            flag     "test flag"           A 
            ipointer "instruction pointer" Q 
        }
    }

    method compile tokens {
        my Compile $tokens
    }

    method Turn {facing {turn left}} {
        switch $turn {
            left  { dict get {e n n w w s s e} $facing }
            front { set facing }
            right { dict get {e s s w w n n e} $facing }
        }
    }

    method _Move {xpos ypos facing} {
        switch $facing {
            e { incr xpos }
            n { incr ypos }
            w { incr xpos -1 }
            s { incr ypos -1 }
        }
        return [list $xpos $ypos]
    }

    method CheckCollision {width height xpos ypos walls} {
        incr width
        incr height
        lappend walls 0 $ypos $width $ypos $xpos 0 $xpos $height
        expr {[list $xpos $ypos] in [lmap {x y} $walls {list $x $y}]}
    }

    method SetFlag {id idx} {
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
                    expr {![my CheckCollision $width $height {*}[my _Move $xpos $ypos [my Turn $facing front]] $walls]}
                }
                left-is-clear {
                    expr {![my CheckCollision $width $height {*}[my _Move $xpos $ypos [my Turn $facing]] $walls]}
                }
                right-is-clear {
                    expr {![my CheckCollision $width $height {*}[my _Move $xpos $ypos [my Turn $facing right]] $walls]}
                }
                next-to-a-beeper {
                    expr {[list $xpos $ypos] in [lmap {x y} $beepers {list $x $y}]}
                }
                facing-east  { expr {$facing eq "e"} }
                facing-north { expr {$facing eq "n"} }
                facing-west  { expr {$facing eq "w"} }
                facing-south { expr {$facing eq "s"} }
                any-beepers-in-beeper-bag { expr {$bag > 0} }
            }
        }
    }

    method ExecCode {id code} {
        lassign $code tag a
        dict with id {
            switch $tag {
                HALT  {
                    return [list flag 0]
                }
                TURN  {
                    return [list facing [my Turn $facing left] flag 0]
                }
                MOVE  {
                    lassign [my _Move $xpos $ypos $facing] xpos ypos
                    if {[my CheckCollision $width $height $xpos $ypos $walls]} {
                        return -code error [format {collision with a wall!}]
                    }
                    return [list xpos $xpos ypos $ypos flag 0]
                }
                TAKE - DROP {
                }
                TEST: {
                    return [list flag [my SetFlag $id $a]]
                }
                RET   {
                    set returns [lassign $returns next]
                    return [list returns $returns ipointer $next flag 0]
                }
                CALL: {
                    set returns [linsert $returns 0 [my vsets succ Q $ipointer]]
                    return [list returns $returns flag 0]
                }
                NOP {
                    return [list flag 0]
                }
            }
        }
    }

    method Exec id {
        log::log d [info level 0] 
        dict with id {
        if {[my vsets in F $ipointer]} {
            return
        }
            set ids [$table map $ipointer {iSym target code} {
                if {$iSym eq $flag} {
                    set d [my ExecCode $id $code]
                    if {[dict exists $d ipointer]} {
                        set target [dict get $d ipointer]
                    } else {
                        dict set d ipointer $target
                    }
                    $iddef make {*}[dict values [dict merge $id $d]]
                } else {
                    continue
                }
            }]
        }
        log::log d \$ids=$ids 
        return $ids
    }

    method Run {world robot beepers walls} {
        #: Run the code with the given configuration, starting from s.
        lappend id {*}$world
        lappend id {*}$robot
        lappend id [list]
        lappend id $beepers
        lappend id $walls
        lappend id 0
        lappend id [my vsets get S]
        set id [$iddef make {*}$id]
        set results [my search $id Exec]
        set res [dict values [lindex $results 0]]
        list [lrange $res 0 1] [lrange $res 2 5] [lindex $res 6] [lindex $res 7] [lindex $res 8] [lindex $res 9] [lindex $res 10]
    }

}

oo::class create ::automata::PTM {
    mixin ::automata::CodeMachine

    variable table iddef

    constructor args {
        my runAs run "run the machine" {tape "a list of symbols"}
        my values A "Tape symbols"     {@ 0 1}
        my values P "Print directives" N+ -hidden 1
        my values M "Move directives"  {@ L R} -hidden 1
        my values Q "Addresses"        N+
        my values S "Start address"    Q
        my values F "Final address"    Q
        my values O "Operations"       #+ -hidden 1
        my values I "Instructions"     {@ JZ: J: PRINT ERASE ROLL:} -hidden 1
        my values V "Values"           N -hidden 1
        my table Q A Q P M O*
        my id {
            tape     "tape contents"       A*
            head     "current index"       V 
            ipointer "instruction pointer" Q 
        }
    }

    method compile tokens {
        ::automata::Compiler compile [self] $tokens
    }

    # Print and Move are borrowed from the BTM

    method ExecCode {id code} {
        log::log d [info level 0] 
        dict with id {
            lassign $code tag a
            switch $tag {
                H  {
                    return
                }
                N {}
            }
        }
        return $id
    }

    method Exec id {
        dict with id {
            if {[my vsets in F $ipointer]} {
                return
            }
            set flag [lindex $tape $head]
            set ids [$table map $ipointer {iSym target print move code} {
                if {$iSym eq $flag} {
                    set d [my ExecCode $id $code]
                    # in a PTM, target always overrides ipointer
                    log::log d \$d=$d 
                    if {$d eq {}} {
                        continue
                    }
                    dict set d ipointer $target
                    if {$print > 0} {
                        dict with d {
                            lset tape $head [lindex [my vsets get A] [expr {$print - 1}]]
                        }
                    }
                    if {$move ne "N"} {
                        dict with d {
                            # PTM has reversed sense of movement
                            my Move tape head [string map {R L L R} $move]
                        }
                    }
                    $iddef make {*}[dict values $d]
                } else {
                    continue
                }
            }]
        }
        return $ids
    }

    method Run tape {
        #: Run the code on this tape, return tape.
        set tape [list {*}$tape]
        set ids [lmap ipointer [my vsets get S] {
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

    variable table iddef

    constructor args {
        my runAs run "run the machine" {}
        my values A "Flag symbols"     {@ 0 1}
        my values Q "Addresses"        N+
        my values S "Start address"    Q
        my values F "Final address"    Q
        my values O "Operations"       #+ -hidden 1
        my values I "Instructions"     {@ JZ: JSZ: JSE: J: PUSH INC DEC CLR DUP EQ EQL ADD MUL} -hidden 1
        my values V "Values"           N -hidden 1
        my table Q A Q O*
        my id {
            stack    "stack contents"      V*
            ipointer "instruction pointer" Q 
        }
    }

    method compile tokens {
        my Compile $tokens
    }

    method SetFlag {stack tag args} {
        lassign $args a b
        lassign $stack TOP
        if {$b == 0} {
            set flag [expr {$TOP == 0}]
        } else {
            set flag [expr {$TOP == [lindex $stack 1]}]
        }
        return $flag
    }

    method ExecCode {stack code} {
        lassign $code tag a b c d e f
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
        return $stack
    }

    method Exec id {
        dict with id {
            if {[my vsets in F $ipointer]} {
                return
            }
            set ids [$table map $ipointer {iSym target code} {
                set f [my SetFlag $stack {*}$code]
                if {$iSym eq $f} {
                    set s [my ExecCode $stack $code]
                    $iddef make $s $target
                } else {
                    continue
                }
            }]
        }
        return $ids
    }

    forward Run my SingleThread Exec

}
