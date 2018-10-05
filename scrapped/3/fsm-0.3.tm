
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
