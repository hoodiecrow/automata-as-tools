::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer

namespace eval automata {}

oo::class create ::automata::FST {
    mixin ::automata::Printer
    variable data epsilon

#: A Finite State Transducer recognizes or encodes a regular relation.

    constructor args {
        set epsilon ε
        #: Recognized options:
        if {[lindex $args 0] eq "-epsilon"} {
            #: -epsilon c when the input symbol for an edge is this character, it is treated as an epsilon move. Default is ε.
            set args [lassign $args - epsilon]
        }
#: This machine is defined by the tuple `<A, B, Q, S, F, T>`:
        ::automata::Component create A -label "Input alphabet" -exclude {}
#: * *A* is the input alphabet (does not accept the empty string as symbol).
        ::automata::Component create B -label "Output alphabet" -exclude {}
#: * *B* is the output alphabet (does not accept the empty string as symbol).
        ::automata::Component create Q -label "State symbols"
#: * *Q* is the set of state symbols.
        ::automata::Component create S -label "Start symbol(s)" -in [namespace which Q]
#: * *S* is a symbol which is a member of the set of state symbols (for a deterministic FST) or a set of symbols which is a subset of the state symbols (for a nondeterministic FST). Processing will start at this/these symbols.
        ::automata::Component create F -label "Final symbol(s)" -in [namespace which Q]
#: * *F* is a set of symbols which is a subset of *Q*. These are the accepting final states.
        ::automata::STE create T [self namespace] {Q A B}
#: * *T* is the transition relation, an instance of the `STE` class.

        #: Inject the processing methods into T.
        oo::objdefine T method recognize id {
            lassign $id a q0 b
            set tuples1 [my get $q0 {}]
            set tuples2 [my get $q0 [lindex $a 0]]
            set tuples [concat $tuples1 $tuples2]
            set _a [lrange $a 1 end]
            set _b [lrange $b 1 end]
            set id [list]
            foreach tuple $tuples {
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset id 0 $a
                } else {
                    lset id 0 $_a
                }
                lset id 1 $q1
                if {$out eq {}} {
                    lset id 2 $b
                } elseif {$out ne [lindex $b 0]} {
                    continue
                } else {
                    lset id 2 $_b
                }
                my addNewIDs $id
            }
        }

        oo::objdefine T method translate id {
            lassign $id a q0 b
            set tuples1 [my get $q0 {}]
            set tuples2 [my get $q0 [lindex $a 0]]
            set tuples [concat $tuples1 $tuples2]
            set _a [lrange $a 1 end]
            set id [list]
            foreach tuple $tuples {
                lassign $tuple - inp q1 out
                if {$inp eq {}} {
                    lset id 0 $a
                } else {
                    lset id 0 $_a
                }
                lset id 1 $q1
                if {$out ne {}} {
                    lset id 2 [list {*}$b [lindex $out 0]]
                } else {
                    lset id 2 $b
                }
                my addNewIDs $id
            }
        }

        oo::objdefine T method reconstruct id {
            lassign $id a q0 b
            set tuples [my getEdges $q0]
            foreach tuple $tuples {
                log::log d \$tuple=$tuple 
                lassign $tuple inp q1 out
                if {$inp ne {}} {
                    lset id 0 [list {*}$a [lindex $inp 0]]
                } else {
                    lset id 0 $a
                }
                lset id 1 $q1
                if {$out eq {}} {
                    lset id 2 $b
                } elseif {$out ne [lindex $b 0]} {
                    continue
                } else {
                    lset id 2 [lrange $b 1 end]
                }
                my addNewIDs $id
            }
        }

        oo::objdefine T method generate id {
            lassign $id a q0 b
            set tuples [my getEdges $q0]
            foreach tuple $tuples {
                log::log d \$tuple=$tuple 
                lassign $tuple inp q1 out
                if {$inp ne {}} {
                    set _a [list {*}$a [lindex $inp 0]]
                } else {
                    set _a $a
                }
                if {$out ne {}} {
                    set _b [list {*}$b [lindex $out 0]]
                } else {
                    set _b $b
                }
                my addNewIDs [list $_a $q1 $_b]
            }
        }

    }

    method compile tokens {
        #: 'source' form is three tokens: from, edge, next.
        #: edge is split by / into input and output
        #: input can contain one or more input symbols, separated by comma.
        foreach {from edge next} $tokens {
            regexp {([\w,]*)\s*/\s*([\w,]*)} $edge -> input output
            splitItems input
            splitItems output
            my T set $from $input $next {*}$output
        }
    }

    #: The ID of an FST is (a, q, b) = current input, current state, and current output.

    method recognize {a b} {
        #: Are we in a final state when all input symbols in a and b are consumed?
        set a [list {*}$a]
        set b [list {*}$b]
        set ids [lmap s [my S get] {list $a $s $b}]
        set results [my T iterate $ids recognize]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        foreach result $results {
            lassign $result a q b
            if {[llength $a] == 0 && [llength $b] == 0} {
                return 1
            }
        }
        return 0
    }

    method translate a {
        #: What symbols have been added to b when all input symbols in a are consumed?
        set a [list {*}$a]
        set ids [lmap s [my S get] {list $a $s {}}]
        set results [my T iterate $ids translate]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct b {
        #: What symbols have been added to a when all input symbols in b are consumed?
        set b [list {*}$b]
        set ids [lmap s [my S get] {list {} $s $b}]
        set results [my T iterate $ids reconstruct]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        #: If we take N steps into the transition sequence (or sequence powerset), what to we get in a and b?
        set ids [lmap s [my S get] {list {} $s {}}]
        set results [my T iterate -steps $steps $ids generate]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        return $results
    }

#: * `A`, `B`, `Q`, `S`, `F`, `T` : public methods to give access to the components.

}
