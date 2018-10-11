
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::machine

namespace eval automata {}

oo::class create ::automata::TableMachine {
    mixin ::automata::Machine

}

oo::class create ::automata::FSM {
    mixin ::automata::TableMachine

    variable table iddef

    constructor args {
        my values A "Input symbols" #+ -epsilon ε
        my values Q "State symbols" #+ -sorted 0
        my values S "Start symbols" Q+
        my values F "Final symbols" Q+
        my table Q A Q
        my id {
            input "remaining input" A*
            state "current state"   Q 
        }
        my runAs accept "accept the input" {input "a list of input symbols"}
        my runAs classify "classify the input" {input "a list of input symbols"}
    }

    method compile tuples {
        #: 'source' form is three tokens: from, input, next.
        #: input can contain one or more input symbols, separated by comma.
        #: Put a `<` character before the state symbol to signify a start
        #: state, and/or a `>` character after it to signify a final state.
        #:
        foreach tokens $tuples {
            foreach {from input next} $tokens {
                foreach inp $input {
                    $table add [my vsets mapped $from < S > F] $inp [my vsets mapped $next < S > F]
                }
            }
        }
    }

    method Exec id {
        dict with id {
            set _tail [lassign $input top]
            set ids [$table map $state {inputSymbol target} {
                if {$inputSymbol eq {}} {
                    # epsilon move: pass input on to next id
                    $iddef make $input $target
                } elseif {$inputSymbol eq $top} {
                    # consuming move: pass input less one symbol to next id
                    $iddef make $_tail $target
                } else {
                    continue
                }
            }]
        }
        return $ids
    }

    method Accept args {
        log::log d [info level 0] 
        # Are we in a final state when all input symbols are consumed?
        lassign $args a
        set input [list {*}$a]
        set ids [lmap state [my vsets get S] {
            $iddef make $input $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my vsets in F $state]} {
                    return 1
                }
            }
        }
        return 0
    }

    method Classify args {
        # What state are we in when all input symbols are consumed?
        lassign $args a
        set input [list {*}$a]
        set ids [lmap state [my vsets get S] {
            $iddef make $input $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my vsets in F $state]} {
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

    variable table iddef

    constructor args {
        my runAs recognize   "recognize two symbol lists" {
            input "a list of symbols"
            output "a list of symbols"
        }
        my runAs translate   "translate a symbol list into another" {
            input "a list of symbols"
        }
        my runAs reconstruct "translate a symbol list into another, backwards" {
            output "a list of symbols"
        }
        my runAs generate "generate two symbol lists for a given number of steps" {steps "number of steps to take"}
        my values A "Input symbols"  #+ -epsilon ε
        my values B "Output symbols" #+ -epsilon ε
        my values Q "State symbols"  #+ -sorted 0
        my values S "Start symbols"  Q+
        my values F "Final symbols"  Q+
        my table Q A Q B
        my id {
            input  "remaining input"  A*
            state  "current state"    Q 
            output "remaining output" B*
        }
    }

    method compile tuples {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and output
        #: input can contain one or more input symbols, separated by comma.
        #:
        foreach tokens $tuples {
            foreach {from edge next} $tokens {
                regexp {(\w+)\s*/\s*(\w+)} $edge -> input output
                foreach inp $input {
                    $table add [my vsets mapped $from < S > F] $inp [my vsets mapped $next < S > F] $output
                }
            }
        }
    }

    method Exec-recognize id {
        dict with id {
            set itail [lassign $input itop]
            set otail [lassign $output otop]
            set ids [$table map $state {iSym target oSym} {
                if {$iSym eq {}} {
                    if {$oSym eq {}} {
                        $iddef make $input $target $output
                    } elseif {$oSym eq $otop} {
                        # consume output token
                        $iddef make $input $target $otail
                    } else {
                        # reject invalid transition
                        continue
                    }
                } elseif {$iSym eq $itop} {
                    if {$oSym eq {}} {
                        # consume input token
                        $iddef make $itail $target $output
                    } elseif {$oSym eq $otop} {
                        # consume input and output token
                        $iddef make $itail $target $otail
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

    method Recognize args {
        # Are we in a final state when all symbols in input and output are consumed?
        lassign $args a b
        set input [list {*}$a]
        set output [list {*}$b]
        set ids [lmap state [my vsets get S] {
            $iddef make $input $state $output
        }]
        log::log d \$ids=$ids 
        set results [concat {*}[lmap id $ids {
            my search $id Exec-recognize
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my vsets in F $state] && [llength $output] == 0} {
                    return 1
                }
            }
        }
        return 0
    }

    method Exec-translate id {
        dict with id {
            set itail [lassign $input itop]
            set ids [$table map $state {iSym target oSym} {
                if {$iSym eq {}} {
                    if {$oSym eq {}} {
                        $iddef make $input $target $output
                    } else {
                        # emit output token
                        $iddef make $input $target [linsert $output end $oSym]
                    }
                } elseif {$iSym eq $itop} {
                    if {$oSym eq {}} {
                        # consume input token
                        $iddef make $itail $target $output
                    } else {
                        # consume input token, emit output token
                        $iddef make $itail $target [linsert $output end $oSym]
                    }
                } else {
                    continue
                }
            }]
        }
    }

    method Translate args {
        # What symbols have been added to output when all input symbols in a are consumed?
        lassign $args a
        set input [list {*}$a]
        set ids [lmap state [my vsets get S] {
            $iddef make $input $state {}
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-translate
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my vsets in F $state]} {
                    set output
                } else {
                    continue
                }
            }
        }
    }

    method Exec-reconstruct id {
        dict with id {
            set otail [lassign $output otop]
            set ids [$table map $state {iSym target oSym} {
                if {$iSym eq {}} {
                    if {$oSym eq {}} {
                        $iddef make $input $target $output
                    } elseif {$oSym eq $otop} {
                        # consume output token
                        $iddef make $input $target $otail
                    } else {
                        # reject invalid transition
                        continue
                    }
                } else {
                    if {$oSym eq {}} {
                        # emit input token
                        $iddef make [linsert $input end $iSym] $target $output
                    } elseif {$oSym eq $otop} {
                        # emit input token, consume output token
                        $iddef make [linsert $input end $iSym] $target $otail
                    } else {
                        # reject invalid transition
                        continue
                    }
                }
            }]
        }
        return $ids
    }

    method Reconstruct args {
        # What symbols have been added to input when all symbols in output are consumed?
        lassign $args b
        set output [list {*}$b]
        set ids [lmap state [my vsets get S] {
            $iddef make {} $state $output
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-reconstruct
        }]]
        lmap result $results {
            dict with result {
                if {[my vsets in F $state] && [llength $output] == 0} {
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
