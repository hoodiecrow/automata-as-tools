::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.2
package require automata::component
package require automata::printer
package require automata::processor
package require automata::robot

namespace eval automata {}

oo::class create ::automata::KTR {
    mixin ::automata::Printer

    #: This is a very limited Karel the Robot that can only walk around, not
    #: interact with beepers.
    #:
    #: The ID of a KTR is (o, r, i, t, b, a) = world, robot, instruction stack, teststate, beepers, walls.

    constructor args {
        #: This machine is defined by the tuple `<A, Q, S, F, T>`:
        #:
        ::automata::Component create A -label "Flag symbols" -domain B
        ::automata::Component create Q -label "Instructions" -domain N
        ::automata::Component create S -label "Program start" -in Q -scalar
        ::automata::Component create F -label "Program end" -in Q -scalar
        S set 0
        ::automata::STE create T
        #: * *T* is the transition relation, an instance of the `STE` class.
        #: 
        #: Inject the Robot class into T.
        oo::objdefine T mixin -append ::automata::Robot

    }

    method compile tokens {
        #: Convert source code to transition configuration.
        #:
        # address 0 is invalid
        set i 1
        set labels {}
        foreach token $tokens {
            if {[string match *: $token]} {
                dict set labels [string trimright $token :] $i
                continue
            }
            if {[regexp {([[:upper:]]+):?(.*)$} $token -> op label]} {
                if {$label eq {}} {
                    set label 0
                }
                set next [expr {$i + 1}]
                switch $op {
                    ACTION {
                        switch $label {
                            turnoff {
                                my F set $i
                                T set $i [A get] 0
                            }
                            turnleft {
                                T set $i [A get] $next TURN
                            }
                            move {
                                T set $i [A get] $next MOVE
                            }
                            pickbeeper {
                                T set $i [A get] $next TAKE
                            }
                            putbeeper {
                                T set $i [A get] $next DROP
                            }
                            default {
                                return -code error [format {unknown action "%s"} $label]
                            }
                        }
                    }
                    TEST { T set $i [A get] $next $op $label }
                    JUMPZ {
                        T set $i 0 $next
                        T set $i 1 $label
                    }
                    JUMP { T set $i [A get] $label }
                    RET { T set $i [A get] 0 $op }
                    GOSUB { T set $i [A get] $label $op }
                    NOP { T set $i [A get] $next }
                    default {
                        ;
                    }
                }
            } else {
                return -code error [format {syntax error: "%s"} $token]
            }
            incr i
        }
        my Q clear
        my Q set {*}[my T fixJumps $labels]
        my S set [lindex $labels 1]
    }

    method run {world robot beepers walls {s {}}} {
        #: Run the code with the given configuration, starting from s.
        if {$s eq {}} {
            set s [my S get]
        }
        lappend id $world
        lappend id $robot
        lappend id $s
        lappend id {}
        lappend id $beepers
        lappend id $walls
        set results [my T iterate [list $id] exec]
        lindex $results 0
    }

#: * `A`, `Q`, `S`, `F`, `T` : public methods to give access to the components.
}
