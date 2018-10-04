::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

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
