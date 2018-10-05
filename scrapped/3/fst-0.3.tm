::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

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
