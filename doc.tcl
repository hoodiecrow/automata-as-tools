package require fileutil

set dir [file dirname [file normalize [info script]]]

set files [::fileutil::findByPattern [file join $dir topdir] -regexp {\.(tm|tcl)$}]

foreach file $files {
    switch -regexp $file {
        {component-.*\.tm$} { set target Component.md }
        {ste-.*\.tm$} { set target STE.md }
        {fsm-.*\.tm$} { set target FSM.md }
        {fst-.*\.tm$} { set target FST.md }
        {pda-.*\.tm$} { set target PDA.md }
        default {
            continue
        }
    }
    set f [open [file join ~/code automata-as-tools.wiki $target] w]
    set nomethods 1
    ::fileutil::foreachLine line $file {
        if {[regexp {^\s*#: (.*)} $line -> m]} {
            puts $f $m
        } elseif {[regexp {\s*oo::class create (\S+)} $line -> name]} {
            puts $f "\n## Name\n\n`$name` (class)\n"
        } elseif {[regexp {\s*constructor} $line]} {
            puts $f "\n## Creation\n"
        } elseif {[regexp {\s*method\s+([[:lower:]]\S*)\s+(.*)\s+\S$} $line -> name arglist]} {
            if {$nomethods} {
                set nomethods 0
                puts $f "\n## Behavior\n"
            }
            set arglist {*}$arglist
            set _arglist {}
            if {[lindex $arglist end] eq "args"} {
                set arglist [lrange $arglist 0 end-1]
                set suffix " ?arg...?"
            } else {
                set suffix {}
            }
            foreach arg $arglist {
                if {[llength $arg] == 1} {
                    append _arglist " $arg"
                } else {
                    append _arglist " ?[lindex $arg 0]?"
                }
            }
            puts $f "\n* `$name$_arglist$suffix`\n"
        }
    }
    close $f
}

if no {
    tk_messageBox -message $files
}

exit
