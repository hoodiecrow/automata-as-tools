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
    constructor args {
        set matrix [::struct::matrix]
        oo::objdefine [self] forward matrix $matrix
        my matrix add columns 5
        foreach script $args {
            oo::objdefine [self] $script
        }
    }
    method AddToken token {
        log::log d [info level 0] 
        # TODO could probably be more elegant
        if {[string match *: $token]} {
            my matrix add row [string trimright $token :]
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
    method Execute {model args} {
        ::automata::Processor create P $model [namespace which my]
        set f [my MakeFrame {*}$args [my GetValues start]]
        P cycle $f
        set result [P extract {*}[my GetFrame]]
        P destroy
        log::log d \$result=$result 
        dict values $result
    }
    method print {} {
        set str {}
        lappend maplist %% %
        lappend maplist %T [my MakeTable {%-6s%-6s%-6s%s %s}]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            Code
            %T
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }

    method dump args {
        list [my matrix serialize] [my GetValues *] [my GetFrame]
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
    method run registers {
        my Execute CM [list {*}$registers]
    }
}

oo::class create ::automata::KTR {
    superclass ::automata::Machine
    constructor args {
        next {*}$args {
            frame world robot returns zflag ipointer
            start 0
        }
    }
    method print {} {
        set str {}
        lappend maplist %% %
        lappend maplist %T [string map {ε -} [my MakeTable {%-12s%-6s%-6s%s %s}]]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            Code
            %T
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method TestClear {dir world robot} {
        lassign $world width height beepers walls
        log::log d [info level 0] 
        switch $dir {
            f { set zflag [expr {![my CheckCollision $world [my Move $robot]]}] }
            l { set zflag [expr {![my CheckCollision $world [my Move [my Turn $robot]]]}] }
            r { set zflag [expr {![my CheckCollision $world [my Move [my Turn [my Turn [my Turn $robot]]]]]}] }
        }
        return $zflag
    }
    method Turn robot {
        lassign $robot xpos ypos bag facing
        set facing [dict get {e n n w w s s e} $facing]
        list $xpos $ypos $bag $facing
    }
    method Move robot {
        lassign $robot xpos ypos bag facing
        switch $facing {
            e { incr xpos }
            n { incr ypos }
            w { incr xpos -1 }
            s { incr ypos -1 }
        }
        list $xpos $ypos $bag $facing
    }
    method CheckCollision {world robot} {
        lassign $world width height beepers walls
        log::log d [info level 0] 
        lassign $robot xpos ypos bag facing
        if {$xpos <= 0 || $ypos <= 0 || $xpos > $width || $ypos > $height} {
            return 1
        } elseif {[list $xpos $ypos] in [lmap {x y} $walls {list $x $y}]} {
            return 1
        } else {
            return 0
        }
    }
    method run {world robot} {
        my Execute KTR $world $robot [list] 0
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
    method run tape {
        my Execute PTM [list {*}$tape] 0
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
    method run stack {
        my Execute SM [list {*}$stack]
    }
}
