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

package require automata::machine
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::TableMachine {
    mixin ::automata::Configuration ::automata::Machine
}

oo::class create ::automata::FSM {
    mixin ::automata::TableMachine

    variable types table iddef

    constructor args {
        my type A "Input symbols" #+ -epsilon ε
        my type Q "State symbols" #+ -sorted 0
        my type S "Start symbols" Q+
        my type F "Final symbols" Q+
        my table1 Q A Q
        my id1 {
            input "remaining input" A*
            state "current state"   Q 
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
        if no {
            my id {
                a A* "remaining input"
                q Q  "current state"
            }
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
                    $types set S [string trimright $from >]
                }
                foreach name {from next} {
                    if {[string match *> [set $name]]} {
                        set $name [string trimright [set $name] >]
                        my add F [set $name]
                        $types set F $from
                    }
                }
                foreach inp $input {
                    $table add $from $inp $next
                    my add table $from $inp $next
                }
            }
        }
    }

    method Exec id {
        # unpack ID
        dict with id {
            # get epsilons
            set targets [lmap row [my get table $state {}] {
                lindex $row 2
            }]
            set ids [lmap target $targets {
                $iddef make $input $target
            }]
            set _tail [lassign $input top]
            set targets [lmap row [my get table $state $top] {
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
        set ids [lmap state [my get S] {
            $iddef make $input $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my in F $state]} {
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
        set ids [lmap state [my get S] {
            $iddef make $input $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my in F $state]} {
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

    variable types table iddef

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
        my type A "Input symbols"  #+ -epsilon ε
        my type B "Output symbols" #+ -epsilon ε
        my type Q "State symbols"  #+ -sorted 0
        my type S "Start symbols"  Q+
        my type F "Final symbols"  Q+
        my table1 Q A Q B
        my id1 {
            input  "remaining input"  A*
            state  "current state"    Q 
            output "remaining output" B*
        }
        my graded "Input symbols"  A -epsilon ε
        my graded "Output symbols" B -epsilon ε
        my graded "State symbols"  Q
        my graded "Start symbols"  S -superset Q
        my graded "Final symbols"  F -superset Q
        my table -as {Q A Q B}
        if no {
            my id {
                a A* "input symbols"
                q Q  "current state"
                b A* "output symbols"
            }
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
                    $types set S [string trimright $from >]
                }
                foreach name {from next} {
                    if {[string match *> [set $name]]} {
                        set $name [string trimright [set $name] >]
                        my add F [set $name]
                        $types set F [set $name]
                    }
                }
                foreach inp $input {
                    $table add $from $inp $next $output
                    my add table $from $inp $next $output
                }
            }
        }
    }

    method Exec-recognize id {
        # unpack ID
        dict with id {
            # get epsilons
            set tuples [my get table $state {}]
            set itail [lassign $input itop]
            set otail [lassign $output otop]
            # get moves
            lappend tuples {*}[my get table $state $itop]
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
        set ids [lmap state [my get S] {
            $iddef make $input $state $output
        }]
        log::log d \$ids=$ids 
        set results [concat {*}[lmap id $ids {
            my search $id Exec-recognize
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my in F $state] && [llength $output] == 0} {
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
            set tuples [my get table $state {}]
            # get moves
            lappend tuples {*}[my get table $state $itop]
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
        set ids [lmap state [my get S] {
            $iddef make $input $state {}
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-translate
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] == 0 && [my in F $state]} {
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
            set tuples [my get table $state *]
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
        set ids [lmap state [my get S] {
            $iddef make {} $state $output
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-reconstruct
        }]]
        lmap result $results {
            dict with result {
                if {[my in F $state] && [llength $output] == 0} {
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
            set tuples [my get table $state *]
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
        set ids [lmap state [my get S] {
            $iddef make {} $state {}
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec-generate $steps
        }]]
        lmap result $results {
            dict with result {
                if {[my in F $state]} {
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

    variable types table iddef

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
        my type A "Input symbols" #+ -epsilon ε
        my type B "Stack symbols" #+ -epsilon ε -sorted 0
        my type Q "State symbols" #+ -sorted 0
        my type S "Start symbol"  Q
        my type Z "Initial stack" B -index 0
        my type F "Final symbols" Q+
        my table1 Q A Q B B*
        my id1 {
            input "remaining input" A*
            state "current state"   Q 
            stack "current stack"   B*
        }
        my graded "Input symbols" A -epsilon ε
        my graded "Stack symbols" B -epsilon ε
        my graded "State symbols" Q
        my graded "Start symbol"  S -superset Q -scalar
        my graded "Initial stack" Z -firstof B -scalar
        my graded "Final symbols" F -superset Q
        my table -as {Q A Q B B*}
        if no {
            my id {
                w A* "remaining input"
                q Q  "current state"
                z B* "current stack"
            }
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
                    $types set S [string trimright $from >]
                }
                foreach name {from next} {
                    if {[string match *> [set $name]]} {
                        set $name [string trimright [set $name] >]
                        my add F [set $name]
                        $types set F [set $name]
                    }
                }
                if {$stackPush eq "ε"} {
                    set stackPush {}
                }
                foreach inp $input {
                    $table add $from $inp $next $stackInput $stackPush
                    my add table $from $inp $next $stackInput $stackPush
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
            set tuples [my get table $state {}]
            # get moves
            lappend tuples {*}[my get table $state $itop]
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
        set stack [list [my get Z]]
        set ids [lmap state [my get S] {
            $iddef make $input $state $stack
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] eq 0 && [my in F $state] && [llength $stack] eq 1} {
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
        set stack [list [my get Z]]
        set ids [lmap state [my get S] {
            $iddef make $input $state $stack
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[llength $input] eq 0 && [my in F $state] && [llength $stack] eq 1} {
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

    variable types table iddef

    constructor args {
        if no {
            runargs {tape "a (part of a) list of tape symbols"}
        }
        my installRunMethod {
            tape {} {a list of initial tape symbols}
            ?head? {} {initial head position}
        }
        my type A "Tape symbols"  #+ -epsilon ε
        my type B "Print symbols" {@ E P N} -sorted 0
        my type C "Move symbols"  {@ L R N} -sorted 0
        my type Q "State symbols" #+ -sorted 0
        my type S "Start symbol"  Q
        my type F "Final symbols" Q+
        my type I "Head position" N+ -index 0
        my table1 Q A Q B C
        my id1 {
            tape  "tape contents" A*
            head  "current index" I 
            state "current state" Q 
        }
        my graded "Tape symbols"  A -sorted
        my graded "Print symbols" B -enum {E P N}
        my graded "Move symbols"  C -enum {L R N}
        my graded "State symbols" Q
        my graded "Start symbol"  S -scalar
        my graded "Final symbols" F
        my graded "Head position" H -domain N -default 0 -scalar
        my table -as {Q A Q B C}
        if no {
            my id {
                t A* "tape"
                h H  "current cell"
                q Q  "current state"
            }
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
                        $types set S [string trimright $from >]
                    }
                    foreach name {from next} {
                        if {[string match *> [set $name]]} {
                            set $name [string trimright [set $name] >]
                            my add F [set $name]
                            $types set F [set $name]
                        }
                    }
                    $table add $from $input $next $print $move
                    my add table $from $input $next $print $move
                } else {
                    return -code error [format {can't parse "%s"} $edge]
                }
            }
        }
    }

    method Print {varName h p} {
        upvar 1 $varName tape
        switch $p {
            N  {}
            E  { lset tape $h [lindex [my get A] 0] }
            P  { lset tape $h [lindex [my get A] 1] }
            default {
                if {[regexp {^P(.)$} $p -> s]} {
                    lset tape $h $s
                }
            }
        }
        return
    }

    method Exec id {
        # unpack ID
        dict with id {
            if {[my in F $state]} {
                return
            }
            # should always be 0 or 1 tuples
            set tuples [my get table $state [lindex $tape $head]]
            set ids [lmap tuple $tuples {
                lassign $tuple - - next print move
                my Print tape $head $print
                my Move tape head $move
                $iddef make $tape $head $next
            }]
        }
        return $ids
    }

    method Run tape {
        #: Run this tape from start index, return tape, current index, and ending state.
        set tape [list {*}$tape]
        set ids [lmap state [my get S] {
            $iddef make $tape [my get H] $state
        }]
        set results [concat {*}[lmap id $ids {
            my search $id Exec
        }]]
        lmap result $results {
            dict with result {
                if {[my in F $state]} {
                    dict values $result
                } else {
                    continue
                }
            }
        }
    }

}
