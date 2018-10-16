package require struct::matrix

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::helpers

namespace eval automata {}

proc ::oo::objdefine::start args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetValues S $args
}

proc ::oo::objdefine::print args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetValues B $args
}

proc ::oo::objdefine::frame args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetFrame $args
}

proc ::oo::objdefine::code body {
    set obj [lindex [info level -1] 1]
    set my [info object namespace $obj]::my
    foreach token [list {*}$body] {
        $my AddToken $token
    }
}

oo::class create ::automata::Machine {
    mixin ::automata::FrameHandler ::automata::ValuesHandler
    variable values frame
    constructor args {
        lassign {} frame
        array set values {}
        ::struct::matrix matrix
        matrix add columns 5
        foreach script $args {
            oo::objdefine [self] $script
        }
        log::log d [matrix serialize]
    }
    method AddToken token {
        if {[string match *: $token]} {
            matrix add row [string trimright $token :]
        } elseif {[string is entier -strict $token]} {
            matrix add row [list {} PUSH $token]
        } else {
            set tuple [regexp -all -inline {(?:[-+]\d+|\w+)} $token]
            log::log d \$tuple=$tuple 
            if {[matrix get cell 1 end] eq {}} {
                set label [matrix get cell 0 end]
                matrix set row end [linsert $tuple 0 $label]
            } else {
                matrix add row [linsert $tuple 0 {}]
            }
        }
    }
    method NextInstruction {ipointer jump} {
        log::log d [info level 0] 
        if {$jump eq {}} {
            incr ipointer
        } elseif {[regexp {[-+]\d+} $jump]} {
            expr $ipointer $jump
        } elseif {![string is integer -strict $jump]} {
            lindex [matrix search column 0 $jump] 0 1
        } else {
            set jump
        }
    }
    method run data {
        log::log d [info level 0] 
        set data [list {*}$data]
        set f [my MakeFrame $data [my GetValues start]]
        dict values [my Execute $f]
    }
}

oo::class create ::automata::CM {
    superclass ::automata::Machine
    constructor args {
        next {*}$args {
            frame registers ipointer
            start 0
            print 0 1
        }
    }
    method Execute f {
        log::log d [info level 0] 
        dict with f {
            while {$ipointer < [matrix rows]} {
                log::log d \$registers=$registers,\ \$ipointer=$ipointer 
                lassign [matrix get row $ipointer] - op a b c
                log::log d [matrix get row $ipointer]
                set jump {}
                log::log d \$op=$op 
                switch $op {
                    INC { lset registers $a [expr {[lindex $registers $a] + 1}] }
                    DEC { lset registers $a [expr {[lindex $registers $a] - 1}] }
                    CLR { lset registers $a 0 }
                    CPY { lset registers $a [lindex $registers $b] }
                    JZ  { if {[lindex $registers $b] eq [lindex $registers 0]} {set jump $a} }
                    JE  { if {[lindex $registers $b] eq [lindex $registers $c]} {set jump $a} }
                    J   { set jump $a }
                    NOP {}
                    default {
                        error \$op=$op 
                    }
                }
                set ipointer [my NextInstruction $ipointer $jump]
                log::log d \$ipointer=$ipointer 
            }
        }
        return $f
    }
}

oo::class create ::automata::KTR {
    superclass ::automata::Machine
    constructor args {
        next {*}$args {
            frame width height xpos ypos bag facing returns beepers walls flag ipointer
            start 0
        }
    }
    method TestClear {dir width height xpos ypos facing walls} {
        switch $dir {
            f {
                set flag [expr {![my CheckCollision $width $height {*}[my Move $xpos $ypos [my Turn $facing front]] $walls]}]
            }
            l {
                set flag [expr {![my CheckCollision $width $height {*}[my Move $xpos $ypos [my Turn $facing]] $walls]}]
            }
            r {
                set flag [expr {![my CheckCollision $width $height {*}[my Move $xpos $ypos [my Turn $facing right]] $walls]}]
            }
        }
        return $flag
    }
    method Turn {facing {turn left}} {
        log::log d [info level 0] 
        switch $turn {
            left  { dict get {e n n w w s s e} $facing }
            front { set facing }
            right { dict get {e s s w w n n e} $facing }
        }
    }
    method Move {xpos ypos facing} {
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
    method Execute f {
        log::log d [info level 0] 
        dict with f {
            while {$ipointer < [matrix rows]} {
                lassign [matrix get row $ipointer] - op a b c
                log::log d [matrix get row $ipointer]
                set jump {}
                set _flag 0
                log::log d \$op=$op 
                switch $op {
                    HALT { break }
                    TEST {
                        switch -glob $a {
                            *_is_clear {
                                set _flag [my TestClear [string index $a 0] $width $height $xpos $ypos $facing $walls]
                            }
                            next_to* {
                                set _flag [expr {[list $xpos $ypos] in [lmap {x y} $beepers {list $x $y}]}]
                            }
                            facing_* {
                                set _flag [expr {$facing eq [string index $a 7]}]
                            }
                            any_* { set _flag [expr {$bag > 0}] }
                            default {
                                return -code error [format {unknown test "%s"} $a]
                            }
                        }
                    }
                    TURN { set facing [my Turn $facing left] }
                    MOVE {
                        lassign [my Move $xpos $ypos $facing] xpos ypos
                        if {[my CheckCollision $width $height $xpos $ypos $walls]} {
                            return -code error [format {collision with a wall!}]
                        }
                    }
                    JZ   { if {$flag eq 0} {set jump $a} }
                    JNZ  { if {$flag ne 0} {set jump $a} }
                    J    { set jump $a }
                    CALL {
                        set returns [linsert $returns 0 $ipointer]
                        set jump $a
                    }
                    RET  { set returns [lassign $returns ipointer] }
                    NOP  {}
                    default {
                        error \$op=$op 
                    }
                }
                set ipointer [my NextInstruction $ipointer $jump]
                set flag $_flag
                log::log d "::> $xpos $ypos $facing ($flag) $ipointer"
            }
        }
        return $f
    }
    method run {world robot beepers walls} {
        log::log d [info level 0] 
        lappend f {*}$world
        lappend f {*}$robot
        lappend f [list]
        lappend f $beepers
        lappend f $walls
        lappend f 0
        lappend f [my GetValues start]
        set result [dict values [my Execute [my MakeFrame {*}$f]]]
        list [lrange $result 0 1] [lrange $result 2 5] {*}[lrange $result 6 end]
    }
}

oo::class create ::automata::PTM {
    superclass ::automata::Machine
    mixin ::automata::TapeHandler
    constructor args {
        next {*}$args {
            frame tape head ipointer
            start 0
            print 0 1
        }
    }
    method Execute f {
        log::log d [info level 0] 
        dict with f {
            while {$ipointer < [matrix rows]} {
                lassign [matrix get row $ipointer] - op a b c
                log::log d [matrix get row $ipointer]
                set jump {}
                set flag [lindex $tape $head]
                switch $op {
                    HALT  { break }
                    PRINT { set tape [my Print $tape $head $a] }
                    ERASE { set tape [my Print $tape $head E] }
                    HEAD  { lassign [my Roll $tape $head [string map {R L L R} $a]] tape head }
                    JZ  - J0 {
                        log::log d \$flag=$flag 
                        if {$flag eq 0} {set jump $a} }
                    JNZ - J1 {
                        log::log d \$flag=$flag 
                        if {$flag ne 0} {set jump $a} }
                    J     { set jump $a }
                    NOP   {}
                    default {
                        error \$op=$op 
                    }
                }
                set ipointer [my NextInstruction $ipointer $jump]
                log::log d \$tape=$tape,\ \$head=$head,\ \$ipointer=$ipointer
            }
        }
        return $f
    }
    method run tape {
        log::log d [info level 0] 
        dict values [my Execute [my MakeFrame $tape 0 [my GetValues start]]]
    }
}

oo::class create ::automata::SM {
    superclass ::automata::Machine
    constructor args {
        next {*}$args {
            frame stack ipointer
            start 0
        }
    }
    method Execute f {
        log::log d [info level 0] 
        dict with f {
            while {$ipointer < [matrix rows]} {
                lassign [matrix get row $ipointer] - op a b c
                log::log d [matrix get row $ipointer]
                set jump {}
                set flag [lindex $stack 0]
                switch $op {
                    HALT  { break }
                    PUSH  { set stack [linsert $stack $a] }
                    ADD   { set stack [lreplace $stack 0 1 [::tcl::mathop::+ {*}[lrange $stack 0 1]]] }
                    MUL   { set stack [lreplace $stack 0 1 [::tcl::mathop::* {*}[lrange $stack 0 1]]] }
                    DEC   { lset stack 0 [expr {[lindex $stack 0] - 1}] }
                    JZ    { if {$flag eq 0} {set jump $a} }
                    J     { set jump $a }
                    NOP   {}
                    default {
                        error \$op=$op 
                    }
                }
                set ipointer [my NextInstruction $ipointer $jump]
            }
        }
        return $f
    }
    method run stack {
        log::log d [info level 0] 
        dict values [my Execute [my MakeFrame $stack [my GetValues start]]]
    }
}
