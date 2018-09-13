::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require automata::ste
package require automata::component

namespace eval automata {}

oo::class create ::automata::PTM {
    variable data instructions

    #: A Post-Turing Machine is essentially a TM controlled by a program
    #: instead of by a state-input->state relation. The tape uses a binary
    #: symbol set (here, {0, 1}).

    constructor args {
        #: This machine is defined by the tuple `<A, b>`:
        ::automata::Component create A -label "Tape alphabet" -exclude {}
        A set 0 1
        #: * *A* is the tape alphabet (does not accept the empty string as symbol).
        ::automata::Component create b -label "Blank symbol" -scalar
        b set 0
        #: * *b* is the blank symbol in the tape alphabet.
    }

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c {A b} {my $c print}] \n]
    }

    method Compile tokens {
        set i 1
        set labels {}
        set instructions [list {}]
        foreach token $tokens {
            switch -regexp -matchvar m $token {
                {([^:]+):$} {
                    dict set labels [lindex $m 1] $i
                    continue
                }
                {(J[01]?):([-+]\d+)$} {
                    lassign $m - op offset
                    set jump [expr {$i + $offset}]
                }
                {([^:]+):(.*)$} {
                    lassign $m - op jump
                }
                default {
                    set op $token
                }
            }
            incr i
            if {$op eq "J"} {
                lappend instructions [list $op $jump]
            } elseif {$op in {J0 J1}} {
                lappend instructions [list $op $i $jump]
            } else {
                lappend instructions [list $op $i]
            }
        }
        for {set i 1} {$i < [llength $instructions]} {incr i} {
            if {[string match J* [lindex $instructions $i 0]]} {
                set label [lindex $instructions $i end]
                if {[dict exists $labels $label]} {
                    lset instructions $i end [dict get $labels $label]
                }
            }
        }
    }

    method run {code tape {tapeIndex 0}} {
        #: Run the code on this tape, return tape.
        my Compile $code
        set ip 1
        # R, L opposite from BTM
        while {[lindex $instructions $ip 0] ne "H"} {
            log::log d \$tape=[lreplace $tape $tapeIndex $tapeIndex *]
            lassign [lindex $instructions $ip] op next jump
            set ip $next
            switch $op {
                P { lset tape $tapeIndex 1 }
                E { lset tape $tapeIndex [b get] }
                R {
                    incr tapeIndex
                    if {$tapeIndex >= [expr {[llength $tape] - 1}]} {
                        lappend tape [b get]
                    }
                }
                L {
                    if {$tapeIndex < 1} {
                        set tape [linsert $tape 0 [b get]]
                    } else {
                        incr tapeIndex -1
                    }
                }
                N - J {}
                J0 { if {[lindex $tape $tapeIndex] eq 0} {set ip $jump} }
                J1 { if {[lindex $tape $tapeIndex] eq 1} {set ip $jump} }
                default {
                    error $op
                }
            }
        }
        return $tape
    }

    foreach m {A b} {
        forward $m $m ; export $m
    }

#: * `A`, `b` : public methods to give access to the components.
}
