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

    method compile tokens {
        ::automata::Compiler compile [self] $tokens
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

    method SetFlag {registers tag args} {
        lassign $args a b
        expr {[lindex $registers $a] eq [lindex $registers $b]}
    }

    method ExecCode {id code} {
        dict with id {
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
        return $id
    }

    method Exec id {
        dict with id {}
        if {[my vsets in F $ipointer]} {
            return
        }
        set ids [$table map $ipointer {iSym target print move code} {
            set flag [my SetFlag $registers {*}$code]
            if {$iSym eq $flag} {
                set d [my ExecCode $id $code]
                dict set d ipointer $target
            } else {
                continue
            }
        }]
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
        my values A "Flag symbols"     {@ 0 1}
        my values B "Facing"           {@ e n w s} -hidden 1
        my values P "Print directives" N+ -hidden 1
        my values M "Move directives"  {@ L R} -hidden 1
        my values Q "Addresses"        N+
        my values S "Start address"    Q
        my values F "Final address"    Q
        my values O "Operations"       #+ -hidden 1
        my values I "Instructions"     {@ JZ: J: TURN MOVE TAKE DROP TEST: RET CALL:} -hidden 1
        my values V "Values"           N -hidden 1
        my table Q A Q P M O*
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

    method Turn {facing {turn left}} {
        switch $turn {
            left  { dict get {e n n w w s s e} $facing }
            front { set facing }
            right { dict get {e s s w w n n e} $facing }
        }
    }

    method _Move {xpos ypos facing} {
        log::log d [info level 0] 
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

    method SetFlag {id code} {
        dict with id {}
        lassign $code tag a b
        switch $tag {
            JC {
                if {$b == 0} {
                    set flag [expr {$flag != 0}]
                } else {
                    return -code error [format {bad conditional jump: %s} $code]
                }
            }
        }
        return $flag
    }

    method ExecCode {id code} {
        lassign $code tag a
        dict with id {
            set flag 0
            set next {}
            switch $tag {
                H  { return -level 2 }
                N { }
                TURN  {
                    set facing [my Turn $facing left]
                }
                MOVE  {
                    lassign [my _Move $xpos $ypos $facing] xpos ypos
                    if {[my CheckCollision $width $height $xpos $ypos $walls]} {
                        return -code error [format {collision with a wall!}]
                    }
                }
                TAKE - DROP {
                }
                T {
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
                    } $a]
                    set flag [switch $label {
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
                    }]
                }
                R   {
                    set returns [lassign $returns next]
                }
                G {
                    log::log d \$ipointer=$ipointer 
                    set returns [linsert $returns 0 [my vsets succ Q $ipointer]]
                }
            }
            log::log d \$flag=$flag 
            set ipointer $next
        }
        return $id
    }

    method Exec id {
        log::log d [info level 0] 
        dict with id {}
        if {[my vsets in F $ipointer]} {
            return
        }
        set ids [$table map $ipointer {iSym target print move code} {
            if {$iSym eq [my SetFlag $id $code]} {
                set d [my ExecCode $id $code]
                if {[dict get $d ipointer] eq {}} {
                    dict set d ipointer $target
                }
                set d
            } else {
                continue
            }
        }]
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

    # Print and Move are borrowed from the BTM

    method ExecCode {id code} {
        log::log d [info level 0] 
        dict with id {
            lassign $code tag a
            switch $tag {
                H  {
                    return -code continue
                }
                N {}
            }
        }
        return $id
    }

    method Exec id {
        dict with id {}
        if {[my vsets in F $ipointer]} {
            return
        }
        set flag [lindex $tape $head]
        set ids [$table map $ipointer {iSym target print move code} {
            if {$iSym eq $flag} {
                set d [my ExecCode $id $code]
                dict with d {
                    # in a PTM, target always overrides ipointer
                    set ipointer $target
                    if {$print > 0} {
                        lset tape $head [lindex [my vsets get A] $print-1]
                    }
                    # PTM has reversed sense of movement
                    my Move tape head [string map {R L L R} $move]
                }
                set d
            } else {
                continue
            }
        }]
        log::log d \$id=$id,\ \$ids=$ids 
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
        my values P "Print directives" N+ -hidden 1
        my values M "Move directives"  {@ L R} -hidden 1
        my values Q "Addresses"        N+
        my values S "Start address"    Q
        my values F "Final address"    Q
        my values O "Operations"       #+ -hidden 1
        my values I "Instructions"     {@ JZ: JSZ: JSE: J: PUSH INC DEC CLR DUP EQ EQL ADD MUL} -hidden 1
        my values V "Values"           N -hidden 1
        my table Q A Q P M O*
        my id {
            stack    "stack contents"      V*
            ipointer "instruction pointer" Q 
        }
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

    method Store {data addr value} {
        log::log d [info level 0] 
        if {$addr eq {}} {
            set addr -1
        }
        lreplace $data -1 $addr $value
    }

    method ExecCode {id code} {
        log::log d [info level 0] 
        lassign $code tag a
        dict with id {
            lassign $stack top sec
            set val [lindex $stack $a]
            switch $tag {
                CP   { lset stack $a $val }
                D    { lset stack $a [expr {$val - 1}] }
                I    { lset stack $a [expr {$val + 1}] }
                CL   { set stack [my Store $stack $a 0] }
                DUP  { set stack [my Store $stack -1 $top] }
                EQ   { set stack [my Store $stack 1 [expr {$top eq $sec}]] }
                EQL  { set stack [my Store $stack 1 [expr {$top == $sec}]] }
                ADD  { set stack [my Store $stack 1 [expr {$top + $sec}]] }
                MUL  { set stack [my Store $stack 1 [expr {$top * $sec}]] }
                PUSH { set stack [my Store $stack -1 $a] }
            }
        }
        return $id
    }

    method Exec id {
        dict with id {}
        if {[my vsets in F $ipointer]} {
            return
        }
        set ids [$table map $ipointer {iSym target print move code} {
            set flag [my SetFlag $stack {*}$code]
            if {$iSym eq $flag} {
                set d [my ExecCode $id $code]
                dict set d ipointer $target
            } else {
                continue
            }
        }]
        return $ids
    }

    forward Run my SingleThread Exec

}
