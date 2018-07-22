
namespace eval ::FSM {
    namespace export *

    namespace eval inner {
        variable params {}
        variable transMat {}
        proc State {name body} {
            namespace eval ::FSM::inner [list interp alias {} transition {} dict set transMat $name]
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
        method run {} {
            set state [dict get $params start]
            set input {}
            while {1} {
                dict for {k v} [dict get $params input] {
                    dict set params input $k [lassign $v token]
                    lappend input $token
                }
                if {{} in $input} {
                    break
                }
                set state [dict get $transMat $state [join $input ,]]
                set input {}
            }
            return [expr [format {"%s" %s} $state [dict get $params accept]]]
        }
    }

    proc Machine {name body} {
        namespace eval ::FSM::inner $body
        CMachine create [uplevel 1 {namespace current}]::$name $inner::params $inner::transMat
    }
}

