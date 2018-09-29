::tcl::tm::path add [file dirname [file dirname [file normalize [info script]]]]

package require -exact automata::ste 0.1
package require automata::component

namespace eval automata {}

oo::class create ::automata::WBM {
    variable data instructions

    #: A Wang B-Machine is an extremely simple Post-Turing-like machine. It has
    #: a unary tape alphabet and only four instructions.

    constructor args {
        #: There are no components.
    }

    method print {} {
        #: There is nothing to print.
    }

    method Compile tokens {
        set i 1
        set labels {}
        set instructions [list {}]
        foreach token $tokens {
            switch -regexp -matchvar m $token {
                {J:(\d+)$} {
                    set op J
                    lassign $m - jump
                    if {$jump < 1 || $jump > [llength $tokens]} {
                        error [format {invalid jump address %d} $jump]
                    }
                }
                default {
                    set op $token
                }
            }
            if {$op eq "J"} {
                lappend instructions [list $i $op $jump]
            } else {
                lappend instructions [list $i $op]
            }
            incr i
        }
    }

    method run {code tape {tapeIndex 0}} {
        #: Run the code on this tape, return tape.
        my Compile $code
        log::log d \$instructions=$instructions 
        set ip 1
        # R, L opposite from BTM
        lassign [lsearch -exact -inline -index 0 $instructions $ip] - op jump
        while {$op in {M L R J}} {
            incr ip
            switch $op {
                M { lset tape $tapeIndex 1 }
                R {
                    incr tapeIndex
                    if {$tapeIndex >= [expr {[llength $tape] - 1}]} {
                        lappend tape 0
                    }
                }
                L {
                    if {$tapeIndex < 1} {
                        set tape [linsert $tape 0 0]
                    } else {
                        incr tapeIndex -1
                    }
                }
                J { if {[lindex $tape $tapeIndex] eq "1"} {set ip $jump} }
                default {
                    error $op
                }
            }
            lassign [lsearch -exact -inline -index 0 $instructions $ip] - op jump
        }
        return $tape
    }

}
