::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::PDA {
    mixin ::automata::Configuration ::automata::Machine

    #: A Pushdown Automaton recognizes a context-free language.
    #:
    #: The configuration of a PDA is (A, B, Q, Z, S, F | w, q, z).

    constructor args {
        my graded "Input symbols" A -epsilon ε
        my graded "Stack symbols" B -epsilon ε
        my graded "State symbols" Q
        my graded "Stack bottom"  Z -scalar
        my graded "Start symbol"  S -scalar
        my graded "Final symbols" F
        my table -as {Q A Q B B*}
        my id {
            w A* "remaining input"
            q Q  "current state"
            z B* "current stack"
        }
    }

    method compile tokens {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and stack-action
        #: input can contain one or more input symbols, separated by comma.
        #: stack-action is split by ; into stack-input and stack-push
        #: Stack symbols in stack-push are separated by commas.
        foreach {from edge next} $tokens {
            regexp {([\w,]*)\s*/\s*(\w*)\s*;\s*([\w,]*)} $edge -> input stackInput stackPush
            splitItems input
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
            foreach inp $input {
                my add table $from $inp $next $stackInput $stackPush
            }
        }
    }

    method run args {
        #: Run the machine:
        set _args [lassign $args arg]
        switch $arg {
            -acceptor - -accept {
                #: provide the flag `-acceptor` or `-accept` to accept input,
                my Accept {*}$_args
            }
            -classifier - -classify {
                #: and `-classifier` or `-classify` to classify input.
                my Classify {*}$_args
            }
            default {
                #: With no flags given, the machine accepts.
                my Accept {*}$args
            }
        }
        #: Also provide a list of input symbols.
    }

    method Accept {a {z {}}} {
        # Are we in a final state when all input symbols are consumed and the stack has only one item?
        set a [list {*}$a]
        if {$z ne {}} {
            my add Z $z
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

    method Classify {a {z {}}} {
        # What state are we in when all input symbols are consumed and the stack has only one item?
        set a [list {*}$a]
        if {$z ne {}} {
            my add Z $z
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
                    set q
                } else {
                    continue
                }
            }
        }
    }

}
