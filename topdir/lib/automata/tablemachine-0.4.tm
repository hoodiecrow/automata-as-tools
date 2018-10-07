
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
        my installRunMethod {
            -accept Accept {[[accept]] the input}
            -classify Classify {[[classify]] the input}
            default Accept {[[accept]] the input}
            a {} {a list of input symbols}
        }
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
        # unpack ID
        dict with id {
            # get epsilons
            set targets [lmap row [$table get $state {}] {
                lindex $row 2
            }]
            set ids [lmap target $targets {
                $iddef make $input $target
            }]
            set _tail [lassign $input top]
            set targets [lmap row [$table get $state $top] {
                lindex $row 2
            }]
            lappend ids {*}[lmap target $targets {
                $iddef make $_tail $target
            }]
        }
        return $ids
    }

    method Accept arglist {
        log::log d [info level 0] 
        # Are we in a final state when all input symbols are consumed?
        lassign $arglist a
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

    method Classify arglist {
        # What state are we in when all input symbols are consumed?
        lassign $arglist a
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
        if no {
            option -recognize "recognize two input/output symbol lists" Recognize
            option -translate "translate an input symbol list and build an output symbol list" Translate
            option -reconstruct "reconstruct an input symbol list from an output symbol list" Reconstruct
            option -generate "generate two input/output symbol lists for a given number of steps"  Generate
            runargs {
                a "(for `-recognize` and `-translate`) an input symbol list"
                b "(for `-recognize` and `-reconstruct`) an output symbol list"
                steps "(for `-generate`) a number of steps"
            }
        }
        my installRunMethod {
            -recognize Recognize {[[recognize]] two input/output symbol lists.}
            -translate Translate {[[translate]] an input symbol list and build an output symbol list.}
            -reconstruct Reconstruct {[[reconstruct]] an input symbol list from an output symbol list.}
            -generate Generate {[[generate]] two input/output symbol lists for a given number of steps.} 
            {a b} {} {(for `-recognize`) two input/output symbol lists}
            a     {} {(for `-translate`) an input symbol list}
            b     {} {(for `-reconstruct`) an output symbol list}
            steps {} {(for `-generate`) a number of steps}
        }
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
        # unpack ID
        dict with id {
            # get epsilons
            set tuples [$table get $state {}]
            set itail [lassign $input itop]
            set otail [lassign $output otop]
            # get moves
            lappend tuples {*}[$table get $state $itop]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # consume input token
                    lset tuple 1 $itail
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } elseif {$out ne $otop} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $otail
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method Recognize arglist {
        # Are we in a final state when all symbols in input and output are consumed?
        lassign $arglist a b
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
        # unpack ID
        dict with id {
            set itail [lassign $input itop]
            # get epsilons
            set tuples [$table get $state {}]
            # get moves
            lappend tuples {*}[$table get $state $itop]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # consume input token
                    lset tuple 1 $itail
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } else {
                    # emit output token
                    lset tuple 3 [linsert $output end [lindex $out 0]]
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method Translate arglist {
        # What symbols have been added to output when all input symbols in a are consumed?
        lassign $arglist a
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
        # unpack ID
        dict with id {
            set otail [lassign $output otop]
            # get moves
            set tuples [$table get $state *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # emit input token
                    lset tuple 1 [linsert $input end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } elseif {$out ne $otop} {
                    # reject invalid transition
                    continue
                } else {
                    # consume output token
                    lset tuple 3 $otail
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method Reconstruct arglist {
        # What symbols have been added to input when all symbols in output are consumed?
        lassign $arglist b
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
        # unpack ID
        dict with id {
            # get moves
            set tuples [$table get $state *]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # emit input token
                    lset tuple 1 [linsert $input end [lindex $inp 0]]
                }
                if {$out eq {}} {
                    lset tuple 3 $output
                } else {
                    # emit output token
                    lset tuple 3 [linsert $output end [lindex $out 0]]
                }
                $iddef make {*}[lrange $tuple 1 end]
            }]
        }
        return $ids
    }

    method Generate arglist {
        # If we take N steps into the transition sequence (or sequence powerset), what do we get in input and output?
        lassign $arglist steps
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
        if no {
            option -accept "accepts the input (default)" Accept
            option -classify "classifies the input" Classify
            runargs {a "a list of input symbols"}
        }
        my installRunMethod {
            -accept Accept {[[accept]] the input}
            -classify Classify {[[classify]] the input}
            default Accept {[[accept]] the input}
            a {} {a list of input symbols}
        }
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
        # unpack ID
        dict with id {
            set itail [lassign $input itop]
            set _tail [lassign $stack _top]
            # get epsilons
            set tuples [$table get $state {}]
            # get moves
            lappend tuples {*}[$table get $state $itop]
            set ids [lmap tuple $tuples {
                # q1 from tuple
                lassign $tuple - inp q1 O _o
                if {$inp eq {}} {
                    lset tuple 1 $input
                } else {
                    # consume input token
                    lset tuple 1 $itail
                }
                if {$O ne $_top} {
                    # reject invalid transition
                    continue
                } else {
                    # push stack
                    lset tuple 3 [concat {*}$_o $_tail]
                }
                # TODO ??
                $iddef make {*}[apply {tuple {
                    set _tail [lassign $tuple - input state stack]
                    list $input $state $stack
                }} $tuple]
            }]
        }
        return $ids
    }

    method Accept arglist {
        # Are we in a final state when all input symbols are consumed and the stack has only one item?
        lassign $arglist a
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

    method Classify arglist {
        # What state are we in when all input symbols are consumed and the stack has only one item?
        lassign $arglist a
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
        if no {
            runargs {tape "initial tape contents"}
        }
        my installRunMethod {
            tape {} {a list of initial tape symbols}
            ?head? {} {initial head position}
        }
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
        # unpack ID
        dict with id {
            if {[my vsets in F $state]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [$table get $state [lindex $tape $head]]
            set ids [lmap tuple $tuples {
                lassign $tuple - - next print move
                my Print tape $head $print
                log::log d \$move=$move 
                my Move tape head $move
                $iddef make $tape $head $next
            }]
        }
        return $ids
    }

    method Run tape {
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
