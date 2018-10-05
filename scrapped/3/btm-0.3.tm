::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::machine 0.3
package require automata::configuration

namespace eval automata {}

if no {
Type big N for naturals, {..} for enumerated, # for any symbol, T+ for subset of T, T for member of T

-index n for #n of type
-sorted for a sorted set
-default x for starting value x
-hidden for left out of documentation 

For table and ID, T* means string of 

type <name> <description> <derivation> ?option...?
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
