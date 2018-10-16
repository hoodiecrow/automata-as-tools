
oo::class create ::automata::TapeHandler {
    method Print {tape head print} {
        if {$head eq -1} {
            set tape [linsert $tape 0 {}]
            set head 0
        }
        switch $print {
            N - 0 { return $tape }
            E { return [lset tape $head [lindex [my GetValues print] 0]] }
            P - {} { return [lset tape $head [lindex [my GetValues print] 1]] }
            default {
                return [lset tape $head [lindex [my GetValues print] $print-1]]
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
            N {}
        }
        return [list $tape $head]
    }
}

oo::class create ::automata::FrameHandler {
    variable frame
    method SetFrame frm {
        set frame $frm
    }
    method MakeFrame args {
        foreach key $frame val $args {
            dict set res $key $val
        }
        return $res
    }
}

oo::class create ::automata::ValuesHandler {
    variable values
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
        return $values($name)
    }
}

