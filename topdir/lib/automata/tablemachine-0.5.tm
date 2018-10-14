
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::machine

namespace eval automata {}

oo::class create ::automata::TableMachine {
    mixin ::automata::Machine

}

oo::class create ::automata::FSM {
    mixin ::automata::TableMachine

    variable values

    constructor args {
        ::struct::matrix matrix
        matrix add columns 3
        foreach tuple [lindex $args 0] {
            set tuple [lassign $tuple keyword]
            switch $keyword {
                table { matrix add row [my MakeTuple $tuple] }
                start { set values(S) [lsort -dict -unique $tuple] }
                final { set values(F) [lsort -dict -unique $tuple] }
            }
        }
        set values(Q) [lsort -dict -unique [concat [matrix get column 0] [matrix get column 2]]]
        set values(A) [lsort -dict -unique [matrix get column 1]]
        log::log d [matrix serialize]
        log::log d [array get values]
    }

    method MakeTuple tuple {
        string map {ε _} $tuple
    }

    method Exec id {
        set ids [list]
        dict with id {
            set tail [lassign $input top]
            for {set row 0} {$row < [matrix rows]} {incr row} {
                lassign [matrix get row $row] q a target
                if {$q eq $state} {
                    if {$a eq "_"} {
                        lappend ids [dict create input $input state $target]
                    } elseif {$a eq $top} {
                        lappend ids [dict create input $tail state $target]
                    }
                }
            }
        }
        return $ids
    }

    method accept args {
        log::log d [info level 0] 
        # Are we in a final state when all input symbols are consumed?
        lassign $args a
        set input [list {*}$a]
        set ids [lmap state $values(S) {
            dict create input $input state $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && $state in $values(F)} {
                    return 1
                }
            }
        }
        return 0
    }

    method classify args {
        # What state are we in when all input symbols are consumed?
        lassign $args a
        set input [list {*}$a]
        set ids [lmap state $values(S) {
            dict create input $input state $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && $state in $values(F)]} {
                    set state
                } else {
                    continue
                }
            }
        }
        return {}
    }

}

oo::class create ::automata::FST {
    mixin ::automata::TableMachine

    variable values

    constructor args {
        ::struct::matrix matrix
        matrix add columns 4
        foreach tuple [lindex $args 0] {
            set tuple [lassign $tuple keyword]
            switch $keyword {
                table { matrix add row [my MakeTuple $tuple] }
                start { set values(S) [lsort -dict -unique $tuple] }
                final { set values(F) [lsort -dict -unique $tuple] }
            }
        }
        set values(Q) [lsort -dict -unique [concat [matrix get column 0] [matrix get column 2]]]
        set values(A) [lsort -dict -unique [matrix get column 1]]
        set values(B) [lsort -dict -unique [matrix get column 3]]
        log::log d [matrix serialize]
        log::log d [array get values]
    }

    method MakeTuple tuple {
        lassign $tuple a b c
        lassign [regexp -all -inline {\w} $b] b d
        set tuple [list $a $b $c $d]
        string map {ε _} $tuple
    }

    method Exec-recognize id {
        set ids [list]
        dict with id {
            set itail [lassign $input itop]
            set otail [lassign $output otop]
            for {set row 0} {$row < [matrix rows]} {incr row} {
                lassign [matrix get row $row] q a target b
                if {$q eq $state} {
                    if {$a eq "_"} {
                        if {$b eq "_"} {
                            lappend ids [dict create input $input state $target output $output]
                        } elseif {$b eq $otop} {
                            lappend ids [dict create input $input state $target output $otail]
                        }
                    } elseif {$a eq $itop} {
                        if {$b eq "_"} {
                            lappend ids [dict create input $itail state $target output $output]
                        } elseif {$b eq $otop} {
                            lappend ids [dict create input $itail state $target output $otail]
                        }
                    }
                }
            }
        }
        return $ids
    }

    method recognize args {
        # Are we in a final state when all symbols in input and output are consumed?
        lassign $args a b
        set input [list {*}$a]
        set output [list {*}$b]
        set ids [lmap state $values(S) {
            dict create input $input state $state output $output
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-recognize
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && $state in $values(F) && [llength $output] == 0} {
                    return 1
                }
            }
        }
        return 0
    }

    method Exec-translate id {
        log::log d [info level 0] 
        set ids [list]
        dict with id {
            set itail [lassign $input itop]
            for {set row 0} {$row < [matrix rows]} {incr row} {
                lassign [matrix get row $row] q a target b
                if {$q eq $state} {
                    if {$a eq "_"} {
                        if {$b eq "_"} {
                            lappend ids [dict create input $input state $target output $output]
                        } else {
                            lappend ids [dict create input $input state $target output [linsert $output end $b]]
                        }
                    } elseif {$a eq $itop} {
                        if {$b eq "_"} {
                            lappend ids [dict create input $itail state $target output $output]
                        } else {
                            lappend ids [dict create input $itail state $target output [linsert $output end $b]]
                        }
                    }
                }
            }
        }
        return $ids
    }

    method translate args {
        # What symbols have been added to output when all input symbols in a are consumed?
        lassign $args a
        set input [list {*}$a]
        set ids [lmap state $values(S) {
            dict create input $input state $state output {}
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-translate
        }]]
        log::log d \$results=$results 
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && $state in $values(F)} {
                    set output
                } else {
                    continue
                }
            }
        }
    }

    method Exec-reconstruct id {
        set ids [list]
        dict with id {
            set otail [lassign $output otop]
            for {set row 0} {$row < [matrix rows]} {incr row} {
                lassign [matrix get row $row] q a target b
                if {$q eq $state} {
                    if {$a eq "_"} {
                        if {$b eq "_"} {
                            lappend ids [dict create input $input state $target output $output]
                        } elseif {$b eq $otop} {
                            lappend ids [dict create input $input state $target output $otail]
                        }
                    } else {
                        if {$b eq "_"} {
                            lappend ids [dict create input [linsert $input end $a] state $target output $output]
                        } else {
                            lappend ids [dict create input [linsert $input end $a] state $target output $otail]
                        }
                    }
                }
            }
        }
        return $ids
    }

    method reconstruct args {
        # What symbols have been added to input when all symbols in output are consumed?
        lassign $args b
        set output [list {*}$b]
        set ids [lmap state $values(S) {
            dict create input {} state $state output $output
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-reconstruct
        }]]
        lmap result $results {
            dict with result {
                if {$state in $values(F) && [llength $output] == 0} {
                    set input
                } else {
                    continue
                }
            }
        }
    }

    method Exec-generate id {
        dict with id {
            set ids [$table map $state {iSym target oSym} {
                if {$iSym eq {}} {
                    if {$oSym eq {}} {
                        $iddef make $input $target $output
                    } else {
                        # emit output token
                        $iddef make $input $target [linsert $output end $oSym]
                    }
                } else {
                    if {$oSym eq {}} {
                        # emit input token
                        $iddef make [linsert $input end $iSym] $target $output
                    } else {
                        # emit input and output token
                        $iddef make [linsert $input end $iSym] $target [linsert $output end $oSym]
                    }
                }
            }]
        }
        return $ids
    }

    method Generate args {
        # If we take N steps into the transition sequence (or sequence powerset), what do we get in input and output?
        lassign $args steps
        set ids [lmap state [my vsets get S] {
            $iddef make {} $state {}
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-generate $steps
        }]]
        lmap result $results {
            dict with result {
                if {[my vsets in F $state]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }

}

oo::class create ::automata::PDA {
    mixin ::automata::TableMachine

    variable table iddef

    constructor args {
        my runAs accept "accept the input" {input "a list of input symbols"}
        my runAs classify "classify the input" {input "a list of input symbols"}
        my values A "Input symbols" #+ -epsilon ε
        my values B "Stack symbols" #+ -epsilon ε -sorted 0
        my values Q "State symbols" #+ -sorted 0
        my values S "Start symbol"  Q
        my values Z "Initial stack" B -index 0
        my values F "Final symbols" Q+
        my table Q A Q B B*
        my id {
            input "remaining input" A*
            state "current state"   Q 
            stack "current stack"   B*
        }
    }

    method compile tuples {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and stack-action
        #: stack-action is split by ; into stack-input and stack-push
        #: Stack symbols in stack-push are separated by commas.
        foreach tokens $tuples {
            foreach {from edge next} $tokens {
                regexp {(\w+)\s*/\s*(\w+)\s*;\s*(.*)} $edge -> input stackInput stackPush
                splitItems stackPush
                if {$stackPush eq "ε"} {
                    set stackPush {}
                }
                foreach inp $input {
                    $table add [my vsets mapped $from < S > F] $inp [my vsets mapped $next < S > F] $stackInput $stackPush
                }
            }
        }
    }

    method Exec id {
        dict with id {
            set itail [lassign $input itop]
            set _tail [lassign $stack _top]
            set ids [$table map $state {iSym target _Sym _Push} {
                if {$iSym eq {}} {
                    if {$_Sym eq {}} {
                        $iddef make $input $target $stack
                    } elseif {$_Sym eq $_top} {
                        # consume stack token
                        $iddef make $input $target [concat $_Push $_tail]
                    } else {
                        # reject invalid transition
                        continue
                    }
                } elseif {$iSym eq $itop} {
                    if {$_Sym eq {}} {
                        # consume input token
                        $iddef make $itail $target $stack
                    } elseif {$_Sym eq $_top} {
                        # consume input and stack token
                        $iddef make $itail $target [concat $_Push $_tail]
                    } else {
                        # reject invalid transition
                        continue
                    }
                } else {
                    # reject invalid transition
                    continue
                }
            }]
        }
        return $ids
    }

    method Accept args {
        # Are we in a final state when all input symbols are consumed and the stack has only one item?
        lassign $args a
        set input [list {*}$a]
        set stack [list [my vsets get Z]]
        set ids [lmap state [my vsets get S] {
            $iddef make $input $state $stack
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] eq 0 && [my vsets in F $state] && [llength $stack] eq 1} {
                    return 1
                }
            }
        }
        return 0
    }

    method Classify args {
        # What state are we in when all input symbols are consumed and the stack has only one item?
        lassign $args a
        set input [list {*}$a]
        set stack [list [my vsets get Z]]
        set ids [lmap state [my vsets get S] {
            $iddef make $input $state $stack
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] eq 0 && [my vsets in F $state] && [llength $stack] eq 1} {
                    set state
                } else {
                    continue
                }
            }
        }
    }

}

oo::class create ::automata::BTM {
    mixin ::automata::TableMachine

    variable table iddef

    constructor args {
        my values A "Tape symbols"  #+ -epsilon ε
        my values B "Print symbols" {@ E P N} -sorted 0
        my values C "Move symbols"  {@ L R N} -sorted 0
        my values Q "State symbols" #+ -sorted 0
        my values S "Start symbol"  Q
        my values F "Final symbols" Q+
        my values I "Head position" N+ -index 0
        my table Q A Q B C
        my id {
            tape  "tape contents" A*
            head  "current index" I 
            state "current state" Q 
        }
        my runAs run "run the machine" {tape "initial tape contents"}
    }

    method compile tuples {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and tape-action
        #: tape-action is split by ; into print and move
        foreach tokens $tuples {
            foreach {from edge next} $tokens {
                if {[regexp {(\w)\s*/\s*([EPN]|P\w)\s*;\s*([LRN])} $edge -> input print move]} {
                    $table add [my vsets mapped $from < S > F] $input [my vsets mapped $next < S > F] $print $move
                } else {
                    return -code error [format {can't parse "%s"} $edge]
                }
            }
        }
    }

    method Print {varName head p} {
        log::log d [info level 0] 
        upvar 1 $varName tape
        switch $p {
            N  {}
            E  { lset tape $head [lindex [my vsets get A] 0] }
            P  { lset tape $head [lindex [my vsets get A] 1] }
            default {
                if {[regexp {^P(.)$} $p -> s]} {
                    lset tape $head $s
                }
            }
        }
        return
    }

    method Exec id {
        log::log d [info level 0] 
        dict with id {
            if {[my vsets in F $state]} {
                return
            }
            set cur [lindex $tape $head]
            set ids [$table map $state {sym target print move} {
                if {$sym eq $cur} {
                    my Print tape $head $print
                    my Move tape head $move
                    $iddef make $tape $head $target
                } else {
                    # reject invalid transition
                    continue
                }
            }]
        }
        return $ids
    }

    method Run tape {
        log::log d [info level 0] 
        #: Run this tape from start index, return tape, current index, and ending state.
        set tape [list {*}$tape]
        set ids [lmap state [my vsets get S] {
            $iddef make $tape [my vsets get I] $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[my vsets in F $state]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }

}
