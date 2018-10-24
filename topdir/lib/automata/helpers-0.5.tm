
oo::class create ::automata::TapeHandler {
    constructor args {next {*}$args}
    method Print {tape head print} {
        if {$head eq -1} {
            set tape [linsert $tape 0 {}]
            set head 0
        }
        set printsyms [my GetValues print]
        switch $print {
            N - _ {}
            E - 0 { lset tape $head [lindex [my GetValues print] 0] }
            P - 1 { lset tape $head [lindex [my GetValues print] 1] }
            default {
                lset tape $head [lindex [my GetValues print] $print]
            }
        }
        return $tape
    }
    method Roll {tape head move} {
        switch $move {
            L {
                if {[lindex $tape [incr head]+1] eq {}} {
                    set tape [my Print $tape end+1 E]
                }
            }
            R {
                if {$head < 1} {
                    set tape [my Print $tape -1 E]
                } else {
                    incr head -1
                }
            }
            N - {} {}
        }
        return [list $tape $head]
    }
}

oo::class create ::automata::FrameHandler {
    variable frame
    constructor args {next {*}$args}
    method SetFrame frm {
        set frame $frm
    }
    method MakeFrame args {
        foreach key $frame val $args {
            dict set res $key $val
        }
        return $res
    }
    method GetFrame {} {
        return $frame
    }
}

oo::class create ::automata::ValuesHandler {
    variable values
    constructor args {
        array set values {}
        next {*}$args
    }
    method SetValues {name {value {}}} {
        set values($name) $value
    }
    method GetValues name {
        switch $name {
            start { set name S }
            final { set name F }
            print { set name B }
            stack { set name Z }
        }
        if {$name ne "*"} {
            if {$values($name) eq {}} {
                if {$name in [my GetLabels]} {
                    set values($name) [my matrix get column [lsearch [my GetLabels] $name]]
                } else {
                    return -code error [format {no such value: %s} $name]
                }
            }
        }
        return [concat {*}[dict values [array get values $name]]]
    }
    method DumpValues {} {
        return [array get values]
    }
}

oo::class create ::automata::PrintHelper {
    constructor args {next {*}$args}
    method MakeMaplist args {
        lappend maplist %% %
        foreach arg $args {
            set val [lsort -unique [concat {*}[lmap name $arg {
                my GetValues $name
            }]]]
            set val [lmap sym $val {
                if {$sym eq "_"} {
                    continue
                } else {
                    set sym
                }
            }]
            lappend maplist %[lindex $arg 0] [join $val ", "]
        }
        return $maplist
    }
    method MakeTable fmt {
        string map {_ ε} [join [lmap row [my matrix get rect 0 0 end end] {
            format $fmt {*}$row
        }] \n]
    }
}
