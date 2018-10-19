package require struct::matrix

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::helpers
package require automata::processor

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
    mixin ::automata::FrameHandler ::automata::ValuesHandler ::automata::PrintHelper
    variable values frame ops
    constructor args {
        lassign {} frame
        array set values {}
        set matrix [::struct::matrix]
        oo::objdefine [self] forward matrix $matrix
        my matrix add columns 5
        foreach script $args {
            oo::objdefine [self] $script
        }
        namespace path {::tcl::mathop}
    }
    method AddToken token {
        log::log d [info level 0] 
        if {[string match *: $token]} {
            my matrix add row [string trimright $token :]
        } elseif {  no  &&  [string is entier -strict $token]} {
            my matrix add row [list {} PUSH $token]
        } else {
            set tuple [regexp -all -inline {(?:[-+]\d+|\w+)} $token]
            if {[my matrix rows] eq 0} {
                my matrix add row {}
            }
            if {[my matrix get cell 1 end] eq {}} {
                set label [my matrix get cell 0 end]
                my matrix set row end [linsert $tuple 0 $label]
            } else {
                my matrix add row [linsert $tuple 0 {}]
            }
        }
    }
    method NextInstruction {ipointer jump} {
        if {$jump eq {}} {
            incr ipointer
        } elseif {[regexp {[-+]\d+} $jump]} {
            expr $ipointer $jump
        } elseif {![string is integer -strict $jump]} {
            lindex [my matrix search column 0 $jump] 0 1
        } else {
            set jump
        }
    }
    method run data {
        set data [list {*}$data]
        set f [my MakeFrame $data [my GetValues start]]
        dict values [my Execute $f]
    }

    method dump args {
        list [my matrix serialize] [array get values] $frame
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
    method print {} {
        set str {}
        set col 0
        lappend %% %
        lappend maplist %T [my MakeTable {%-6s%-6s%-6s%s %s}]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            Code
            %T
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method Execute f {
        ::automata::Processor create P CM [namespace which my]
        P cycle $f
        set result [P extract {*}[my GetFrame]]
        P destroy
        return $result
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
    method print {} {
        set str {}
        set col 0
        lappend %% %
        lappend maplist %T [string map {ε -} [my MakeTable {%-12s%-6s%-6s%s %s}]]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            Code
            %T
            {Instantaneous description: %D}
        } \n]]
        puts $str
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
        switch $turn {
            left  { dict get {e n n w w s s e} $facing }
            front { set facing }
            right { dict get {e s s w w n n e} $facing }
        }
    }
    method Move {xpos ypos facing} {
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
        dict with f {
            while {$ipointer < [my matrix rows]} {
                lassign [my matrix get row $ipointer] - op a b c
                set jump {}
                set _flag 0
                switch $op {
                    HALT { break }
                    TEST {
                        switch $a {
                            front - left - right {
                                set _flag [my TestClear [string index $a 0] $width $height $xpos $ypos $facing $walls]
                            }
                            next {
                                set _flag [expr {[list $xpos $ypos] in [lmap {x y} $beepers {list $x $y}]}]
                            }
                            facing {
                                set _flag [expr {$facing eq [string index $b 0]}]
                            }
                            any { set _flag [expr {$bag > 0}] }
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
            }
        }
        return $f
    }
    method run {world robot beepers walls} {
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
    method print {} {
        set str {}
        set col 0
        lappend %% %
        lappend maplist %T [string map {ε -} [my MakeTable {%-12s%-6s%-6s%s %s}]]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            Code
            %T
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method Execute f {
        ::automata::Processor create P PTM [namespace which my]
        P cycle $f
        set result [P extract {*}[my GetFrame]]
        P destroy
        return $result
    }
    method run tape {
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
    method print {} {
        set str {}
        set col 0
        lappend %% %
        lappend maplist %T [string map {ε -} [my MakeTable {%-6s%-6s%-6s%s %s}]]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            Code
            %T
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method Execute f {
        ::automata::Processor create P SM [namespace which my]
        P cycle $f
        set result [P extract {*}[my GetFrame]]
        P destroy
        return $result
    }
    method run stack {
        dict values [my Execute [my MakeFrame $stack [my GetValues start]]]
    }
}
