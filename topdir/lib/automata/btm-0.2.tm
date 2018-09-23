::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::operator

namespace eval automata {}

oo::class create ::automata::BTM {
    mixin ::automata::Printer

    #: A Basic Turing Machine recognizes a recursively enumerable language.
    #:
    #: The ID of a BTM is (t, q, h) = current tape, current state, and current head.

    constructor args {
        #: This machine is defined by the tuple `<A, B, b, Q, S, F, T>`:
        ::automata::Component create A -label "Tape symbols" -exclude {{} L R}
        ::automata::Component create B -label "Init symbols" -exclude {{}} -in [namespace which A]
        ::automata::Component create b -label "Blank symbol" -scalar -in [namespace which A]
        ::automata::Component create Q -label "State symbols"
        ::automata::Component create S -label "Start symbol" -in [namespace which Q] -scalar
        ::automata::Component create F -label "Final symbols" -in [namespace which Q]
        ::automata::STE create T {Q S F B A}
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the Blank method and Operator class into T.
        oo::objdefine T method Blank {} [format {
            return [eval %s]
        } [list [namespace which b] get]]

        oo::objdefine T mixin -append ::automata::Operator

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

    method run {tape {tapeIndex 0}} {
        #: Run this tape from this position, return tape, current position, and ending state.
        set tape [list {*}$tape]
        set ids [list [list $tape [my S get] $tapeIndex]]
        set results [my T iterate $ids process]
        return [lselect result {[my F contains [lindex $result 1]]} $results]
    }

#: * `A`, `B`, `b`, `Q`, `S`, `F`, `T` : public methods to give access to the components.
}
