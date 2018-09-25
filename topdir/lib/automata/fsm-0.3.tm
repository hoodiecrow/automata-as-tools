
::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

if no {
    package require -exact automata::ste 0.3
}
package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::FSM {
    mixin ::automata::Configuration ::automata::Machine

    #: A [[Finite State Machine|finitestatemachine]] recognizes a regular
    #: language. It can be asked to accept (respond with 1 if the machine
    #: recognizes the input, otherwise 0) or classify (respond with the final
    #: state) a list of input symbols.
    #:
    #: The configuration of an FSM is (A, Q, S, F, T | w, q)

    constructor args {
        my graded "Input symbols" A -epsilon ε
        my graded "State symbols" Q
        my graded "Start symbols" S -insert Q
        my graded "Final symbols" F -insert Q
        my table -as {Q A Q}
        my id {w q} {A* Q}
    }

    method compile tokens {
        #: 'source' form is three tokens: from, input, next.
        #: input can contain one or more input symbols, separated by comma.
        #: Put a `<` character before the state symbol to signify a start
        #: state, and/or a `>` character after it to signify a final state.
        #:
        foreach {from input next} $tokens {
            splitItems input
            if no {
                set input [lmap inp $input {if {$inp eq {ε}} list {set inp}}]
            }
            lassign {} qss qfs
            if {[string match <* $from]} {
                set from [string trimleft $from <]
                lappend qss [string trimright $from >]
            }
            foreach name {from next} {
                if {[string match *> [set $name]]} {
                    set $name [string trimright [set $name] >]
                    lappend qfs [set $name]
                }
            }
            foreach inp $input {
                my add table $from $inp $next
            }
            my add A {*}$input
            my add Q $from $next
            my add S {*}$qss
            my add F {*}$qfs
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

    method Accept a {
        # Are we in a final state when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap q [my get S] {
            my AddID $a $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id consumeOne
        }]]
        lmap result $results {
            dict with result {
                if {[llength $w] == 0 && [my in F $q]} {
                    return 1
                }
            }
        }
        return 0
    }

    method Classify a {
        # What state are we in when all input symbols are consumed?
        set a [list {*}$a]
        set ids [lmap q [my get S] {
            my AddID $a $q
        }]
        set results [concat {*}[lmap id $ids {
            my search $id consumeOne
        }]]
        lmap result $results {
            dict with result {
                if {[llength $w] == 0 && [my in F $q]} {
                    set q
                } else {
                    continue
                }
            }
        }
        return {}
    }

#: * `A`, `Q`, `S`, `F`, `T` : public methods to give access to the components.

}
