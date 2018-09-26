::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::FST {
    mixin ::automata::Configuration ::automata::Machine

    #: A Finite State Transducer recognizes or encodes a regular relation.
    #:
    #: The configuration of an FST is (A, B, Q, S, F, T | a, q, b)

    constructor args {
        my graded "Input symbols"  A -epsilon ε
        my graded "Output symbols" B -epsilon ε
        my graded "State symbols"  Q
        my graded "Start symbols"  S
        my graded "Final symbols"  F
        my table -as {Q A Q B}
        my id {
            a A* "input symbols"
            q Q  "current state"
            b A* "output symbols"
        }
    }

    method compile tokens {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and output
        #: input can contain one or more input symbols, separated by comma.
        #:
        foreach {from edge next} $tokens {
            regexp {([\w,]*)\s*/\s*([\w,]*)} $edge -> input output
            splitItems input
            splitItems output
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

    method run args {
        #: Run the machine:
        set _args [lassign $args arg]
        switch $arg {
            -recognize {
                #: provide the flag `-recognize` to recognize input/output stacks,
                my Recognize {*}$_args
            }
            -translate {
                #: provide the flag `-translate` to translate input and build output,
                my Translate {*}$_args
            }
            -reconstruct {
                #: provide the flag `-reconstruct` to reconstruct input from output,
                my Reconstruct {*}$_args
            }
            -generate {
                #: provide the flag `-generate` to generate input/output stacks for a given number of steps,
                my Generate {*}$_args
            }
            default {
                return -code error [format {provide a flag option to specify how to run the machine}]
            }
        }
        #: Also provide a list of input symbols.
    }

    method Recognize {a b} {
        # Are we in a final state when all input symbols in a and b are consumed?
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

    method Translate a {
        # What symbols have been added to b when all input symbols in a are consumed?
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

    method Reconstruct b {
        # What symbols have been added to a when all input symbols in b are consumed?
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

    method Generate steps {
        # If we take N steps into the transition sequence (or sequence powerset), what do we get in a and b?
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
