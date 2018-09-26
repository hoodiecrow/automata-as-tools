::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

oo::class create ::automata::BTM {
    mixin ::automata::Configuration ::automata::Machine

    #: A Basic Turing Machine recognizes a recursively enumerable language.
    #:
    #: The configuration of a BTM is (A, B, C, Q, E, S, F, H | t h q)

    constructor args {
        my graded "Input symbols" A -epsilon ε -exclude {L R}
        my graded "Print symbols" B -exclude {L R}
        my graded "Move symbols"  C -enum {L R N}
        my graded "State symbols" Q
        my graded "Erase symbol"  E -scalar
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

    method compile tokens {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and tape-action
        #: tape-action is split by ; into print and move
        foreach {from edge next} $tokens {
            regexp {([\w,]*)\s*/\s*(\w*)\s*;\s*([LRN])} $edge -> input print move
            splitItems input
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
                my add table $from $inp $next $print $move
            }
        }
    }

    method run {tape {tapeIndex {}}} {
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
