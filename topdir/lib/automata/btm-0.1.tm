::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::ste
package require automata::component

namespace eval automata {}

oo::class create ::automata::BTM {
    variable data

    #: A Basic Turing Machine recognizes a recursively enumerable language.

    constructor args {
        #: This machine is defined by the tuple `<A, B, b, Q, S, F, T>`:
        ::automata::Component create A -label "Tape alphabet" -exclude {{} L R}
        #: * *A* is the tape alphabet (does not accept the empty string as symbol).
        ::automata::Component create B -label "Init alphabet" -exclude {} -in [namespace which A]
        #: * *B* is the initial alphabet (does not accept the empty string as symbol).
        ::automata::Component create b -label "Blank symbol" -scalar
        #: * *b* is the blank symbol in the tape alphabet.
        ::automata::Component create Q -label "State symbols"
        #: * *Q* is the set of state symbols.
        ::automata::Component create S -label "Start symbol(s)" -in [namespace which Q] -scalar
        #: * *S* is a symbol which is a member of the set of state symbols. The processor will be preloaded with this symbol.
        ::automata::Component create F -label "Final symbol(s)" -in [namespace which Q]
        #: * *F* is a set of symbols which is a subset of *Q*. These are the accepting final states.
        ::automata::STE create T [self namespace] {Q B A}
        #: * *T* is the transition relation, an instance of the `STE` class.
    }

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c {A B b Q S F T} {my $c print}] \n]
    }

    method run {tape {tapeIndex 0}} {
        #: Run this tape from this position, return tape, current position, and ending state.
        set results [my T iterate [linsert $tape 0 $tapeIndex] [my S get] {} [my F get] MatchTape PrintMove NoOp]
        # there should only be one
        lassign $results result
        lassign $result tape q
        return [list [lassign $tape tapeIndex] $tapeIndex $q]
    }

    foreach m {A B b Q S F T} {
        forward $m $m ; export $m
    }

#: * `A`, `B`, `b`, `Q`, `S`, `F`, `T` : public methods to give access to the components.
}
