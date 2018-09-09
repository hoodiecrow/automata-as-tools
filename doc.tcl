package require fileutil

set dir [file dirname [file normalize [info script]]]

set files [::fileutil::findByPattern [file join $dir topdir] -regexp {\.(tm|tcl)$}]

foreach file $files {
    switch -regexp $file {
        {component.*\.tm$} {
            set target component.md
        }
        default {
            continue
        }
    }
    set f [open [file join ~/code automata-as-tools.wiki $target] w]
    ::fileutil::foreachLine line $file {
        if {[regexp {^\s*#: (.*)} $line -> m]} {
            puts $f $m
        }
    }
    close $f
}

tk_messageBox -message $files

exit
