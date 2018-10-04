if no {
Type big N for naturals, {..} for enumerated, # for any symbol, T+ for subset of T, T for member of T

-index n for #n of type
-sorted for a sorted set
-default x for starting value x
-hidden for left out of documentation 

For table and ID, T* means string of 

type <name> <description> <derivation> ?option...?
}

::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::Configuration ::automata::Machine

    constructor args {
        if no {
            option -accept "accepts the input (default)" Accept
            option -classify "classifies the input" Classify
            default Accept
            runargs {a "a list of input symbols"}
            type A "Input symbols" # -epsilon ε
            type Q "State symbols" #
            type S "Start symbols" Q+
            type F "Final symbols" Q+
            table Q A Q
            id {
                input A* "remaining input"
                state Q  "current state"
            }
        }
        my installRunMethod {
            -accept Accept {[[accept]] the input}
            -classify Classify {[[classify]] the input}
            default Accept {[[accept]] the input}
            a {} {a list of input symbols}
        }
        my graded "Input symbols" A -epsilon ε
        my graded "State symbols" Q
        my graded "Start symbols" S -superset Q
        my graded "Final symbols" F -superset Q
        my table -as {Q A Q}
        my id {
            a A* "remaining input"
            q Q  "current state"
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
                if no {
                    splitItems input
                }
                if {[string match <* $from]} {
                    set from [string trimleft $from <]
                    my add S [string trimright $from >]
                }
                foreach name {from next} {
                    if {[string match *> [set $name]]} {
                        set $name [string trimright [set $name] >]
                        my add F [set $name]
                    }
                }
                foreach inp $input {
                    my add table $from $inp $next
                }
            }
        }
    }

    method Accept arglist {
        log::log d [info level 0] 
        # Are we in a final state when all input symbols are consumed?
        lassign $arglist a
        set a [list {*}$a]
        set ids [lmap q [my get S] {
            my AddID $a $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id consumeOne
        }]]
        lmap result $results {
            dict with result {
                if {[llength $a] == 0 && [my in F $q]} {
                    return 1
                }
            }
        }
        return 0
    }

    method Classify arglist {
        # What state are we in when all input symbols are consumed?
        lassign $arglist a
        set a [list {*}$a]
        set ids [lmap q [my get S] {
            my AddID $a $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id consumeOne
        }]]
        lmap result $results {
            dict with result {
                if {[llength $a] == 0 && [my in F $q]} {
                    set q
                } else {
                    continue
                }
            }
        }
        return {}
    }

}

oo::class create ::automata::FST {
    mixin ::automata::Configuration ::automata::Machine

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
            type A "Input symbols"  # -epsilon ε
            type B "Output symbols" # -epsilon ε
            type Q "State symbols"  # -sorted
            type S "Start symbols"  Q+
            type F "Final symbols"  Q+
            table Q A Q B
            id {
                input  A* "remaining input"
                state  Q  "current state"
                output B* "remaining output"
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
        my graded "Input symbols"  A -epsilon ε
        my graded "Output symbols" B -epsilon ε
        my graded "State symbols"  Q
        my graded "Start symbols"  S -superset Q
        my graded "Final symbols"  F -superset Q
        my table -as {Q A Q B}
        my id {
            a A* "input symbols"
            q Q  "current state"
            b A* "output symbols"
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
                if no {
                    splitItems input
                    splitItems output
                }
                if {[string match <* $from]} {
                    set from [string trimleft $from <]
                    my add S [string trimright $from >]
                }
                foreach name {from next} {
                    if {[string match *> [set $name]]} {
                        set $name [string trimright [set $name] >]
                        my add F [set $name]
                    }
                }
                foreach inp $input {
                    my add table $from $inp $next $output
                }
            }
        }
    }

    method Recognize arglist {
        # Are we in a final state when all input symbols in a and b are consumed?
        lassign $arglist a b
        set a [list {*}$a]
        set b [list {*}$b]
        set ids [lmap q [my get S] {
            my AddID $a $q $b
        }]
        set results [concat {*}[lmap id $ids {
            my search $id recognize
        }]]
        foreach result $results {
            dict with result {
                if {[llength $a] == 0 && [my in F $q] && [llength $b] == 0} {
                    return 1
                }
            }
        }
        return 0
    }

    method Translate arglist {
        # What symbols have been added to b when all input symbols in a are consumed?
        lassign $arglist a
        set a [list {*}$a]
        set ids [lmap q [my get S] {
            my AddID $a $q {}
        }]
        set results [concat {*}[lmap id $ids {
            my search $id translate
        }]]
        lmap result $results {
            dict with result {
                if {[llength $a] == 0 && [my in F $q]} {
                    set b
                } else {
                    continue
                }
            }
        }
    }

    method Reconstruct arglist {
        # What symbols have been added to a when all input symbols in b are consumed?
        lassign $arglist b
        set b [list {*}$b]
        set ids [lmap q [my get S] {
            my AddID {} $q $b
        }]
        set results [concat {*}[lmap id $ids {
            my search $id reconstruct
        }]]
        lmap result $results {
            dict with result {
                if {[my in F $q] && [llength $b] == 0} {
                    set a
                } else {
                    continue
                }
            }
        }
    }

    method Generate arglist {
        # If we take N steps into the transition sequence (or sequence powerset), what do we get in a and b?
        lassign $arglist steps
        set ids [lmap q [my get S] {
            my AddID {} $q {}
        }]
        set results [concat {*}[lmap id $ids {
            my search $id generate $steps
        }]]
        lmap result $results {
            dict with result {
                if {[my in F $q]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }

}

oo::class create ::automata::PDA {
    mixin ::automata::Configuration ::automata::Machine

    constructor args {
        if no {
            option -accept "accepts the input (default)" Accept
            option -classify "classifies the input" Classify
            runargs {a "a list of input symbols"}
            type A "Input symbols" # -epsilon ε
            type B "Stack symbols" # -epsilon ε
            type Q "State symbols" # -sorted
            type S "Start symbol"  Q
            type Z "Initial stack" [tindex B 0]
            type F "Final symbols" Q+
            table Q A Q B B*
            id {
                input A* "remaining input"
                state Q  "current state"
                stack B* "current stack"
            }
        }
        my installRunMethod {
            -accept Accept {[[accept]] the input}
            -classify Classify {[[classify]] the input}
            default Accept {[[accept]] the input}
            a {} {a list of input symbols}
        }
        my graded "Input symbols" A -epsilon ε
        my graded "Stack symbols" B -epsilon ε
        my graded "State symbols" Q
        my graded "Start symbol"  S -superset Q -scalar
        my graded "Initial stack" Z -firstof B -scalar
        my graded "Final symbols" F -superset Q
        my table -as {Q A Q B B*}
        my id {
            w A* "remaining input"
            q Q  "current state"
            z B* "current stack"
        }
    }

    method compile tuples {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and stack-action
        #: input can contain one or more input symbols, separated by comma.
        #: stack-action is split by ; into stack-input and stack-push
        #: Stack symbols in stack-push are separated by commas.
        foreach tokens $tuples {
            foreach {from edge next} $tokens {
                regexp {(\w+)\s*/\s*(\w+)\s*;\s*(.*)} $edge -> input stackInput stackPush
                if no {
                    splitItems input
                }
                splitItems stackPush
                if {[string match <* $from]} {
                    set from [string trimleft $from <]
                    my add S [string trimright $from >]
                }
                foreach name {from next} {
                    if {[string match *> [set $name]]} {
                        set $name [string trimright [set $name] >]
                        my add F [set $name]
                    }
                }
                if {$stackPush eq "ε"} {
                    set stackPush {}
                }
                foreach inp $input {
                    my add table $from $inp $next $stackInput $stackPush
                }
            }
        }
    }

    method Accept arglist {
        # Are we in a final state when all input symbols are consumed and the stack has only one item?
        lassign $arglist a
        set a [list {*}$a]
        if no {
            set Z [lindex [my get B] 0]
        }
        set ids [lmap q [my get S] {
            my AddID $a $q [list [my get Z]]
        }]
        set results [concat {*}[lmap id $ids {
            my search $id makeMoves
        }]]
        lmap result $results {
            dict with result {
                if {[llength $w] eq 0 && [my in F $q] && [llength $z] eq 1} {
                    return 1
                }
            }
        }
        return 0
    }

    method Classify arglist {
        # What state are we in when all input symbols are consumed and the stack has only one item?
        lassign $arglist a
        set a [list {*}$a]
        set Z [lindex [my get B] 0]
        set ids [lmap q [my get S] {
            my AddID $a $q [list $Z]
        }]
        set results [concat {*}[lmap id $ids {
            my search $id makeMoves
        }]]
        lmap result $results {
            dict with result {
                if {[llength $w] eq 0 && [my in F $q] && [llength $z] eq 1} {
                    set q
                } else {
                    continue
                }
            }
        }
    }

}

oo::class create ::automata::BTM {
    mixin ::automata::Configuration ::automata::Machine

    constructor args {
        if no {
            runargs {tape "a (part of a) list of tape symbols"}
            type A "Tape symbols"  {0 1}
            type B "Print symbols" {0 1} -epsilon N
            type C "Move symbols"  {L R} -epsilon N
            type Q "State symbols" #  -sorted
            type E "Blank symbol"  [tindex B 0]
            type S "Start symbol"  Q+
            type F "Final symbol"  Q+
            table Q A Q B C
            id {
                tape  A* "tape contents"
                head  N  "current index"
                state Q  "current state"
            }
        }
        my installRunMethod {
            tape {} {a list of initial tape symbols}
            ?head? {} {initial head position}
        }
        my graded "Tape symbols"  A -sorted
        my graded "Print symbols" B -enum {E P N}
        my graded "Move symbols"  C -enum {L R N}
        my graded "State symbols" Q
        my graded "Start symbol"  S -scalar
        my graded "Final symbols" F
        my graded "Head position" H -domain N -default 0 -scalar
        my table -as {Q A Q B C}
        my id {
            t A* "tape"
            h H  "current cell"
            q Q  "current state"
        }
    }

    method compile tuples {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and tape-action
        #: tape-action is split by ; into print and move
        foreach tokens $tuples {
            foreach {from edge next} $tokens {
                if {[regexp {(\w)\s*/\s*([EPN]|P\w)\s*;\s*([LRN])} $edge -> input print move]} {
                    if {[string match <* $from]} {
                        set from [string trimleft $from <]
                        my add S [string trimright $from >]
                    }
                    foreach name {from next} {
                        if {[string match *> [set $name]]} {
                            set $name [string trimright [set $name] >]
                            my add F [set $name]
                        }
                    }
                    my add table $from $input $next $print $move
                } else {
                    return -code error [format {can't parse "%s"} $edge]
                }
            }
        }
    }

    method Run {tape {tapeIndex {}}} {
        #: Run this tape from this position, return tape, current position, and ending state.
        if {$tapeIndex ne {}} {
            my add H $tapeIndex
        }
        set tape [list {*}$tape]
        set ids [lmap q [my get S] {
            my AddID $tape [my get H] $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id process
        }]]
        lmap result $results {
            dict with result {
                if {[my in F $q]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }

}
