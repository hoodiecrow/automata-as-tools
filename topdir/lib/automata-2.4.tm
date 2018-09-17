
::tcl::tm::path add [file dirname [file normalize [info script]]]

if no {
    package require automata::fsm
    package require automata::fst
    package require automata::pda
    package require automata::btm
    package require automata::ptm
}

# change output function to have four slots:
# q0 sym q1
# q0  *   * output on leave
#  *  *  q1 output on enter
# q0  *  q1 output on move
# q0 sym  * output on transit

if no {
    apply {args {
            set dir [file dirname [file normalize [info script]]]
            foreach arg $args {
                source -encoding utf-8 [file join $dir .. src $arg]
            }
    }} set.tcl
}

proc splitItems varName {
    upvar 1 $varName var
    upvar 1 epsilon epsilon
    set var [lmap symbol [split $var ,] {
        set symbol [string trim $symbol]
        if {[info exists epsilon] && $symbol eq $epsilon} {
            set symbol {}
        }
        set symbol
    }]
}

proc lselect {varName cond items} {
    upvar 1 $varName item
    return [lmap item $items {
        if [uplevel 1 [list expr $cond]] {
            set item
        } else {
            continue
        }
    }]
}

proc ::tcl::dict::group {varName key args} {
    upvar 1 $varName var
    dict lappend var $key $args
}
namespace ensemble configure dict -map [dict merge [namespace ensemble configure dict -map] {group ::tcl::dict::group}]

