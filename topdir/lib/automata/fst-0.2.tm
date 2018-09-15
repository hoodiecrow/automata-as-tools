::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component

namespace eval automata {}

oo::class create ::automata::FST {
    variable data

#: A Finite State Transducer recognizes or encodes a regular relation.

    constructor args {
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
    }

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c {A B Q S F T} {my $c print}] \n]
    }

    #: The ID of an FST is (a, q, b) = current input, current state, and current output.

    method Recognize id {
        lassign $id a q0 b
        set tuples1 [my T get $q0 {}]
        set tuples2 [my T get $q0 [lindex $a 0]]
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
            my T addNewIDs $id
        }
    }

    method Translate id {
        lassign $id a q0 b
        set tuples1 [my T get $q0 {}]
        set tuples2 [my T get $q0 [lindex $a 0]]
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
            my T addNewIDs $id
        }
    }

    method Reconstruct id {
        lassign $id a q0 b
        set tuples [my T getEdges $q0]
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
            my T addNewIDs $id
        }
    }

    method Generate id {
        lassign $id a q0 b
        set tuples [my T getEdges $q0]
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
            my T addNewIDs [list $_a $q1 $_b]
        }
    }

    method recognize {a b} {
        #: Are we in a final state when all input symbols in a and b are consumed?
        set a [list {*}$a]
        set b [list {*}$b]
        set ids [lmap s [my S get] {list $a $s $b}]
        set results [my T iterate $ids [namespace code [list my Recognize]]]
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
        set results [my T iterate $ids [namespace code [list my Translate]]]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        return [lmap result [lselect result {[llength [lindex $result 0]] == 0} $results] {
            lindex $result 2
        }]
    }

    method reconstruct b {
        #: What symbols have been added to a when all input symbols in b are consumed?
        set b [list {*}$b]
        set ids [lmap s [my S get] {list {} $s $b}]
        set results [my T iterate $ids [namespace code [list my Reconstruct]]]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        return [lmap result [lselect result {[llength [lindex $result 2]] == 0} $results] {
            lindex $result 0
        }]
    }

    method generate steps {
        #: If we take N steps into the transition sequence (or sequence powerset), what to we get in a and b?
        set ids [lmap s [my S get] {list {} $s {}}]
        set results [my T iterate -steps $steps $ids [namespace code [list my Generate]]]
        set results [lselect result {[lindex $result 1] in [my F get]} $results]
        return $results
    }

    foreach m {A B Q S F T} {
        forward $m $m ; export $m
    }
#: * `A`, `B`, `Q`, `S`, `F`, `T` : public methods to give access to the components.

}
