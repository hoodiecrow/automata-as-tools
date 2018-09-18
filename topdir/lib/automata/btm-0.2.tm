::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::processor

namespace eval automata {}

oo::class create ::automata::BTM {
    mixin ::automata::Printer
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

        #: Inject the Blank method and Processor class into T.
        oo::objdefine T method Blank {} [format {
            return [eval %s]
        } [list [namespace which b] get]]

        oo::objdefine T mixin -append ::automata::Processor

    }

    method compile tokens {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and tape-action
        #: tape-action is split by ; into print and move
        foreach {from edge next} $tokens {
            regexp {([\w,]*)\s*/\s*(\w*)\s*;\s*(\w*)} $edge -> input print move
            splitItems input
            if {$print eq {}} {
                set print N
            }
            if {$move eq {}} {
                set move N
            }
            my T set $from $input $next $print $move
        }
    }

    #: The ID of a BTM is (t, q, h) = current tape, current state, and current head.

    method run {tape {tapeIndex 0}} {
        #: Run this tape from this position, return tape, current position, and ending state.
        set tape [list {*}$tape]
        set ids [list [list $tape [my S get] $tapeIndex]]
        set results [my T iterate $ids process]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
    }

#: * `A`, `B`, `b`, `Q`, `S`, `F`, `T` : public methods to give access to the components.
}
