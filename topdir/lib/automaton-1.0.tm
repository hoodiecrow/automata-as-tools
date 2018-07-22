
namespace eval ::FSM {
    namespace export *

    namespace eval inner {
        variable params {}
        variable transMat {}
        proc reset {} {
            variable params {}
            variable transMat {}
        }
        proc State {name body} {
            namespace eval ::FSM::inner [list interp alias {} transition {} apply {{name input state} {
                namespace upvar ::FSM::inner transMat transMat
                if {[dict exists $transMat $name $input]} {
                    dict with transMat $name {
                        lappend $input $state
                    }
                } else {
                    dict set transMat $name $input [list $state]
                }
            }} $name]
            namespace eval ::FSM::inner $body
            namespace eval ::FSM::inner [list interp alias {} transition {}]
        }
        interp alias {} start {} dict set params start
        interp alias {} accept {} dict set params accept
        interp alias {} input {} dict set params input
    }

    oo::class create CMachine {
        variable params transMat state
        constructor args {
            lassign $args params transMat
        }
        method set args {
            dict set params {*}$args
        }
        method accept states {
            log::log d [info level 0] 
            foreach state $states {
                log::log d [format {"%s" %s} $state [dict get $params accept]]
                if [format {"%s" %s} $state [dict get $params accept]] {
                    return 1
                }
            }
            return 0
        }
        method run {} {
            set states [list [dict get $params start]]
            lassign $states state
            log::log d transMat=$transMat
            log::log d input=[dict get $params input]
            while {1} {
                set input {}
                dict for {k v} [dict get $params input] {
                    dict set params input $k [lassign $v token]
                    lappend input $token
                }
                if {{} in $input} {
                    return [my accept $states]
                }
                if {[llength $states] eq 0} {
                    return 0
                }
                # ε-moves
                set symbol ε
                foreach state $states {
                    if {[dict exists $transMat $state $symbol]} {
                        lappend states {*}[dict get $transMat $state $symbol]
                    }
                }
                set states [lsort -unique $states]
                set nextStates {}
                set symbol [join $input ,]
                foreach state $states {
                    log::log d \$state=$state 
                    if {[dict exists $transMat $state $symbol]} {
                        lappend nextStates {*}[dict get $transMat $state $symbol]
                    }
                }
                log::log d \$nextStates=$nextStates 
                set states $nextStates
            }
        }
    }

    proc Machine {name body} {
        inner::reset
        namespace eval ::FSM::inner $body
        CMachine create [uplevel 1 {namespace current}]::$name $inner::params $inner::transMat
    }
}

