
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

oo::class create ::automata::LabelsHandler {
    constructor args {next {*}$args}
    method SetLabels args {
        set labels $args
        foreach v $labels {
            my SetValues $v
        }
        set labels [::automata::Labels new {*}$args]
        foreach m {get dump} {
            oo::objdefine [self] forward [string totitle $m]Labels $labels $m
        }
    }
}

oo::class create ::automata::Labels {
    variable labels
    constructor args {
        set labels $args
    }
    method get {} {
        return $labels
    }
    forward dump my get
}

oo::class create ::automata::FrameHandler {
    constructor args {next {*}$args}
    method SetFrame args {
        set frame [::automata::Frame new {*}$args]
        foreach m {make get dump} {
            oo::objdefine [self] forward [string totitle $m]Frame $frame $m
        }
    }
}

oo::class create ::automata::Frame {
    variable frame
    constructor args {
        set frame $args
    }
    method make args {
        foreach key $frame val $args {
            dict set res $key $val
        }
        return $res
    }
    method get {} {
        return $frame
    }
    forward dump my get
}

oo::class create ::automata::ValuesHandler {
    constructor args {
        set values [::automata::Values new]
        foreach m {add set get dump} {
            oo::objdefine [self] forward [string totitle $m]Values $values $m
        }
        next {*}$args
    }
}

oo::class create ::automata::Values {
    variable values
    constructor args {
        array set values {}
    }
    method add {name value} {
        if {$value ni $values($name)} {
            set values($name) [lsort -dict [concat $values($name) $value]]
        }
    }
    method set {name {value {}}} {
        set values($name) $value
    }
    method get name {
        switch $name {
            start { set name S }
            final { set name F }
            print { set name B }
            stack { set name Z }
        }
        return $values($name)
    }
    method dump {} {
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
