
::tcl::tm::path add [file dirname [file normalize [info script]]]

package require automata::fsm

if no {
    apply {args {
            set dir [file dirname [file normalize [info script]]]
            foreach arg $args {
                source -encoding utf-8 [file join $dir .. src $arg]
            }
    }} set.tcl
}
