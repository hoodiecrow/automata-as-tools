package require struct::matrix

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::helpers

namespace eval automata {}

proc ::oo::objdefine::table args {
    set obj [lindex [info level -1] 1]
    set my [info object namespace $obj]::my
    $my SetLabels {*}$args
    $my matrix add columns [llength $args]
}

proc ::oo::objdefine::tuples args {
    set obj [lindex [info level -1] 1]
    set my [info object namespace $obj]::my
    foreach tuple $args {
        set fields [regexp -all -inline {(?:,|\w)+} $tuple]
        $my matrix add row $fields
        if {![$my GetOptions -epsilon] && [lindex $fields 1] eq "_"} {
            return -code error [format "machine not configured for epsilon moves"]
        }
        foreach label [$my GetLabels] field $fields {
            $my AddValues $label $field
        }
    }
}

proc ::oo::objdefine::start args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetValues S $args
}

proc ::oo::objdefine::final args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetValues F $args
}

proc ::oo::objdefine::print args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetValues B $args
}

proc ::oo::objdefine::stack args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetValues Z $args
}

proc ::oo::objdefine::options args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetOptions {*}$args
}

proc ::oo::objdefine::frame args {
    set obj [lindex [info level -1] 1]
    [info object namespace $obj]::my SetFrame {*}$args
}

oo::class create ::automata::Automaton {
    mixin ::automata::ValuesHandler ::automata::LabelsHandler ::automata::PrintHelper ::automata::FrameHandler ::automata::OptionsHandler
    constructor args {
        oo::objdefine [self] forward matrix [::struct::matrix]
        foreach script [lreverse $args] {
            oo::objdefine [self] $script
        }
    }
    method SelectQ {varNames state body} {
        foreach name $varNames {
            upvar 1 $name $name
        }
        for {set row 0} {$row < [my matrix rows]} {incr row} {
            lassign [my matrix get row $row] {*}$varNames
            if {[my matrix get cell 0 $row] eq $state} {uplevel 1 $body}
        }
    }
    method RecognizeA f {
        set fs [list]
        dict with f {
            if {[my GetOptions -halting] && $state in [my GetValues final]} {
                return
            }
            set tail [lassign $input top]
            my SelectQ {q a t} $state {
                set _input [switch $a "_" {set input} $top {set tail} default {continue}]
                lappend fs [my MakeFrame $_input $t]
            }
        }
        return $fs
    }
    method RecognizeAB f {
        set fs [list]
        dict with f {
            if {[my GetOptions -halting] && $state in [my GetValues final]} {
                return
            }
            set itail [lassign $input itop]
            set otail [lassign $output otop]
            my SelectQ {q a b t} $state {
                set _input [switch $a "_" {set input} $itop {set itail} default {continue}]
                set _output [switch $b "_" {set output} $otop {set otail} default {continue}]
                lappend fs [my MakeFrame $_input $t $_output]
            }
        }
        return $fs
    }
    method TranslateAB f {
        set fs [list]
        dict with f {
            if {[my GetOptions -halting] && $state in [my GetValues final]} {
                return
            }
            set itail [lassign $input itop]
            my SelectQ {q a b t} $state {
                set _input [switch $a "_" {set input} $itop {set itail} default {continue}]
                set _output [switch $b "_" {set output} default {linsert $output end $b}]
                lappend fs [my MakeFrame $_input $t $_output]
            }
        }
        return $fs
    }
    method TranslateBA f {
        set fs [list]
        dict with f {
            if {[my GetOptions -halting] && $state in [my GetValues final]} {
                return
            }
            set otail [lassign $output otop]
            my SelectQ {q a b t} $state {
                set _input [switch $a "_" {set input} default {linsert $input end $a}]
                set _output [switch $b "_" {set output} $otop {set otail} default {continue}]
                lappend fs [my MakeFrame $_input $t $_output]
            }
        }
        return $fs
    }
    method Generate f {
        set fs [list]
        dict with f {
            if {[my GetOptions -halting] && $state in [my GetValues final]} {
                return
            }
            my SelectQ {q a b t} $state {
                set _input [switch $a "_" {set input} default {linsert $input end $a}]
                set _output [switch $b "_" {set output} default {linsert $output end $b}]
                lappend fs [my MakeFrame $_input $t $_output]
            }
        }
        return $fs
    }
    method RecognizeStack f {
        set fs [list]
        dict with f {
            if {[my GetOptions -halting] && $state in [my GetValues final]} {
                return
            }
            set itail [lassign $input itop]
            set _tail [lassign $stack _top]
            my SelectQ {q a b bs t} $state {
                if {$bs eq "_"} {
                    set bs {}
                }
                set _stack [concat [split $bs ,] $_tail]
                set _input [switch $a "_" {set input} $itop {set itail} default {continue}]
                set _stack [switch $b "_" {set stack} $_top {set _stack} default {continue}]
                lappend fs [my MakeFrame $_input $t $_stack]
            }
        }
        return $fs
    }
    method RecognizeTape f {
        set fs [list]
        dict with f {
            if {[my GetOptions -halting] && $state in [my GetValues final]} {
                return
            }
            set cur [lindex $tape $head]
            my SelectQ {q a p m t} $state {
                if {$a eq $cur} {
                    set _tape [my Print $tape $head $p]
                    lassign [my Roll $_tape $head $m] _tape _head
                    lappend fs [my MakeFrame $_tape $_head $t]
                }
            }
        }
        return $fs
    }

    method Search {f fn {steps {}}} {
        if {$steps ne {}} {
            if {$steps <= 0} {
                return [list $f]
            } else {
                incr steps -1
            }
        }
        set fs [my $fn $f]
        if {[llength $fs] eq 0} {
            return [list $f]
        }
        set fs [lsort -unique $fs]
        return [concat {*}[lmap f $fs {
            my Search $f $fn $steps
        }]]
    }

    method dump args {
        list [my DumpLabels] [my matrix serialize] [my DumpValues] [my DumpFrame]
    }
}

oo::class create ::automata::FSM {
    superclass ::automata::Automaton
    constructor args {
        next {*}$args {
            table Q A T
            frame input state
            options -halting 0 -epsilon 1
        }
    }
    method print {} {
        set str {}
        set maplist [my MakeMaplist A {Q T} S F]
        lappend maplist %T [my MakeTable {%1$s, %2$s → %3$s}]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            {Input symbols     A = {%A}}
            {State symbols     Q = {%Q}}
            {Start symbols     S = {%S}}
            {Final symbols     F = {%F}}
            Transitions
            %T
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method accept args {
        # Are we in a final state when all input symbols are consumed?
        lassign $args a
        set input [list {*}$a]
        set fs [lmap state [my GetValues start] {
            my MakeFrame $input $state
        }]
        set results [concat {*}[lmap f $fs {
            my Search $f RecognizeA
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && $state in [my GetValues final]} {
                    return 1
                }
            }
        }
        return 0
    }
}

oo::class create ::automata::FST {
    superclass ::automata::Automaton
    constructor args {
        next {*}$args {
            table Q A B T
            frame input state output
            options -halting 0 -epsilon 1
        }
    }
    method print {} {
        set str {}
        set col 0
        set maplist [my MakeMaplist A B {Q T} S F]
        lappend maplist %T [my MakeTable {%1$s, %2$s → %4$s}]
        lappend maplist %O [my MakeTable {%1$s, %2$s → %3$s}]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            {Input symbols     A = {%A}}
            {Output symbols    B = {%B}}
            {State symbols     Q = {%Q}}
            {Start symbols     S = {%S}}
            {Final symbols     F = {%F}}
            Transitions
            %T
            Output
            %O
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method recognize args {
        # Are we in a final state when all symbols in input and output are consumed?
        lassign $args a b
        set input [list {*}$a]
        set output [list {*}$b]
        set fs [lmap state [my GetValues start] {
            my MakeFrame $input $state $output
        }]
        set results [concat {*}[lmap f $fs {
            my Search $f RecognizeAB
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && $state in [my GetValues final] && [llength $output] == 0} {
                    return 1
                }
            }
        }
        return 0
    }
    method translate args {
        # What symbols have been added to output when all input symbols in a are consumed?
        lassign $args a
        set input [list {*}$a]
        set fs [lmap state [my GetValues start] {
            my MakeFrame $input $state {}
        }]
        set results [concat {*}[lmap f $fs {
            my Search $f TranslateAB
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && $state in [my GetValues final]} {
                    set output
                } else {
                    continue
                }
            }
        }
    }
    method reconstruct args {
        # What symbols have been added to input when all symbols in output are consumed?
        lassign $args b
        set output [list {*}$b]
        set fs [lmap state [my GetValues start] {
            my MakeFrame {} $state $output
        }]
        set results [concat {*}[lmap f $fs {
            my Search $f TranslateBA
        }]]
        lmap result $results {
            dict with result {
                if {$state in [my GetValues final] && [llength $output] == 0} {
                    set input
                } else {
                    continue
                }
            }
        }
    }
    method generate args {
        # If we take N steps into the transition sequence (or sequence powerset), what do we get in input and output?
        lassign $args steps
        set fs [lmap state [my GetValues start] {
            my MakeFrame {} $state {}
        }]
        set results [concat {*}[lmap f $fs {
            my Search $f Generate $steps
        }]]
        lmap result $results {
            dict with result {
                if {$state in [my GetValues final]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }
}

oo::class create ::automata::PDA {
    superclass ::automata::Automaton
    constructor args {
        next {*}$args {
            table Q A B B* T
            frame input state stack
            options -halting 0 -epsilon 1
        }
    }
    method print {} {
        set str {}
        set maplist [my MakeMaplist A B {Q T} Z S F]
        lappend maplist %T [my MakeTable {%1$s, %2$s, %3$s → %5$s}]
        lappend maplist %O [my MakeTable {%1$s, %2$s, %3$s → %4$s}]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            {Input symbols     A = {%A}}
            {Output symbols    B = {%B}}
            {State symbols     Q = {%Q}}
            {Initial stack     Z = %Z}
            {Start symbol      S = %S}
            {Final symbols     F = {%F}}
            Transitions
            %T
            Output
            %O
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method accept args {
        # Are we in a final state when all input symbols are consumed and the stack has only one item?
        lassign $args a
        set input [list {*}$a]
        set stack [list [my GetValues stack]]
        set fs [lmap state [my GetValues start] {
            my MakeFrame $input $state $stack
        }]
        set results [concat {*}[lmap f $fs {
            my Search $f RecognizeStack
        }]]
        lmap result $results {
            dict with result {
                if {
                    [llength $input] eq 0 &&
                    $state in [my GetValues final] &&
                    [llength $stack] eq 1
                } {
                    return 1
                }
            }
        }
        return 0
    }
}

oo::class create ::automata::BTM {
    superclass ::automata::Automaton
    mixin ::automata::TapeHandler
    constructor args {
        next {*}$args {
            table Q A P M T
            frame tape head state
            print 0 1
            options -halting 1 -epsilon 0
        }
    }
    method print {} {
        set str {}
        set maplist [my MakeMaplist {A B} {Q T} P M S F]
        lappend maplist %T [my MakeTable {%1$s, %2$s → %5$s}]
        lappend maplist %O [my MakeTable {%1$s, %2$s → %3$s %4$s}]
        lappend maplist %D [join [my GetFrame] ", "]
        append str [string map $maplist [join {
            {Tape symbols      A = {%A}}
            {State symbols     Q = {%Q}}
            {Start symbol      S = %S}
            {Final symbols     F = {%F}}
            Transitions
            %T
            Tape\ actions
            %O
            {Instantaneous description: %D}
        } \n]]
        puts $str
    }
    method run {{tape {0}}} {
        #: Run this tape from start index, return tape, current index, and ending state.
        set tape [list {*}$tape]
        set fs [lmap state [my GetValues start] {
            my MakeFrame $tape 0 $state
        }]
        set results [concat {*}[lmap f $fs {
            my Search $f RecognizeTape
        }]]
        lmap result $results {
            dict with result {
                if {$state in [my GetValues final]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }
}

