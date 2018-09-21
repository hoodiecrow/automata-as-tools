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
        {btm-.*\.tm$} { set target BTM.md }
        {ptm-.*\.tm$} { set target PTM.md }
        {cm-.*\.tm$}  { set target CM.md }
        {sm-.*\.tm$}  { set target SM.md }
        {ktr-.*\.tm$} { set target KTR.md }
        default {
            continue
        }
    }
    set f [open [file join ~/code automata-as-tools.wiki $target] w]
    set nomethods 1
    ::fileutil::foreachLine line $file {
        switch -regexp -matchvar m $line {
            {^\s*#:(.*)} {
                puts $f [string trimleft [lindex $m 1]]
            }
            {^\s*oo::class create (\S+)} {
                puts $f "\n## Name\n\n`[lindex $m 1]` (class)\n"
            }
            {^\s*constructor} {
                puts $f "\n## Creation\n"
            }
            {^\s*method\s+([[:lower:]]\S*)\s+(.*)\s+\S$} {
                lassign $m - name arglist
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
            {Component create\s+(\w+)\s+(.*)} {
                lassign $m - name optlist
                set label {}
                set isscalar 0
                set cmd {}
                set domain {}
                set exclude {}
                set optlist [regsub {\[namespace which (\w+)\]} $optlist {\1}]
                while {[llength $optlist]} {
                    switch [lindex $optlist 0] {
                        -label {
                            set optlist [lassign $optlist - label]
                        }
                        -scalar {
                            set optlist [lassign $optlist -]
                            set isscalar 1
                        }
                        -in {
                            set optlist [lassign $optlist - cmd]
                        }
                        -domain {
                            set optlist [lassign $optlist - domain]
                        }
                        -exclude {
                            set optlist [lassign $optlist - exclude]
                        }
                        default {
                            break
                        }
                    }
                    set s "* *$name* is the "
                    if {!$isscalar} {
                        append s "set of "
                    }
                    append s [string tolower $label] " "
                    if {[llength $exclude]} {
                        append s "(excluding "
                        foreach sym $exclude {
                            if {$sym eq {}} {
                                append s "the empty string, "
                            } else {
                                append s "$sym, "
                            }
                        }
                        set s [string replace $s end-1 end-1 )]
                    }
                    if {$cmd ne {}} {
                        if {$isscalar} {
                            append s "(∈ $cmd)"
                        } else {
                            append s "(⊆ $cmd)"
                        }
                    }
                    switch $domain {
                        B { append s "(⊂ {0, 1})" }
                        N { append s "(⊂ ℕ : 0, 1, 2, ...)" }
                        Z { append s "(⊂ ℤ : ..., −2, −1, 0, 1, 2, ...)" }
                        R { append s "(⊂ ℝ : real numbers)" }
                        default {
                            ;
                        }
                    }
                }
                puts $f [string trimright $s].
            }
            default {
                ;
            }
        }
    }
    close $f
}

if no {
    tk_messageBox -message $files
}

exit
