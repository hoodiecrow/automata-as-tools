
namespace eval automata {}

oo::object create ::automata::Compiler
oo::objdefine ::automata::Compiler {
    variable machine addr program jumps

    method compile args {
        lassign $args machine tokens
        #: Convert source code to transition configuration.
        unset -nocomplain program jumps
        set addr 0
        foreach token $tokens {
            my AddOp $token
        }
        my GetOps
    }

    method AddOp token {
        log::log d [info level 0] 
        if {![info exists jumps]} {
            dict set jumps BEG_OF_CODE $addr
        }
        if {![regexp {(\w+:?)(.*)} $token -> cmd lbl]} {
            return -code error [format {syntax error: %s} $token]
        }
        set lblargs [regexp -all -inline {[-+\w]+} $lbl]
        if {[string match *: $cmd] && [llength $lblargs] eq 0} {
            dict set jumps [string trimright $cmd :] $addr
        } else {
            dict set program $addr cmd $cmd
            dict set program $addr lbl $lblargs
            dict set jumps END_OF_CODE [incr addr]
        }
    }

    method GetOps {} {
        $machine vsets set S [dict get $jumps BEG_OF_CODE]
        $machine vsets set F [dict get $jumps END_OF_CODE]
        dict for {addr data} $program {
            dict with data {
                lassign $lbl a b c d e f
                if no {
                    set next [expr {$addr + 1}]
                }
                if {[regexp {^[-+]\d+$} $a]} {
                    set a [expr $addr$a]
                } elseif {[dict exists $jumps $a]} {
                    set a [dict get $jumps $a]
                }
                lassign [my compile$cmd $addr $a $b $c $d $e $f] addresses printop rollop code
                set code [lmap c $code {
                    if {$c eq {}} {
                        continue
                    } else {
                        set c
                    }
                }]
                foreach inp [$machine vsets get A] next $addresses {
                    $machine addTable $addr $inp $next $printop $rollop $code
                }
            }
        }
    }

    method compileNOP {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list N]]
    }

    method compileJ: {addr args} {
        lassign $args a
        return [list [list $a $a] N N [list N]]
    }

    method compileCALL: {addr args} {
        lassign $args a
        return [list [list $a $a] N N [list G]]
    }

    method compileRET {addr args} {
        set end [$machine vsets get F]
        return [list [list $end $end] N N [list R]]
    }

    method compileJNZ: {addr args} {
        lassign $args a b
        set next [expr {$addr + 1}]
        return [list [list $a $next] N N [list JC $b 0]]
    }

    method compileJZ: {addr args} {
        lassign $args a b
        set next [expr {$addr + 1}]
        return [list [list $next $a] N N [list JC $b 0]]
    }

    method compileJNE: {addr args} {
        lassign $args a b c
        set next [expr {$addr + 1}]
        return [list [list $a $next] N N [list JC $b $c]]
    }

    method compileJE: {addr args} {
        lassign $args a b c
        set next [expr {$addr + 1}]
        return [list [list $next $a] N N [list JC $b $c]]
    }

    method compileHALT {addr args} {
        set end [my vsets get F]
        return [list [list $end $end] N N [list H]]
    }

    method compileTEST {addr args} {
        lassign $args lbl
        set next [expr {$addr + 1}]
        set code [lsearch -exact {
            front-is-clear
            left-is-clear
            right-is-clear
            next-to-a-beeper
            facing-north
            facing-south
            facing-east
            facing-west
            any-beepers-in-beeper-bag
        } $lbl]
        return [list [list $next $next] N N [list T $code]]
    }

    method compilePRINT: {addr args} {
        lassign $args a
        set next [expr {$addr + 1}]
        return [list [list $next $next] $a N {}]
    }

    method compilePRINT {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] 1 N {}]
    }

    method compileERASE {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] 0 N {}]
    }

    method compileROLL: {addr args} {
        lassign $args a
        set next [expr {$addr + 1}]
        return [list [list $next $next] N $a {}]
    }

    method compileINC: {addr args} {
        lassign $args a
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list I $a]]
    }

    method compileINC {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list I 0]]
    }

    method compileDEC: {addr args} {
        lassign $args a
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list D $a]]
    }

    method compileDEC {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list D 0]]
    }

    method compileCLR: {addr args} {
        lassign $args a
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list CL $a]]
    }

    method compileCPY: {addr args} {
        lassign $args a b
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list CP $a $b]]
    }

    method compilePUSH {addr args} {
        lassign $args val
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list PUSH $val]]
    }

    method compileDUP {addr args} {
        lassign $args val
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list CP 0 0]]
    }

    method compileEQ {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list EQ]]
    }

    method compileEQL {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list EQL]]
    }

    method compileADD {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list ADD]]
    }

    method compileMUL {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list MUL]]
    }

    method compileTURN {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list TURN]]
    }

    method compileMOVE {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list MOVE]]
    }

    method compileTAKE {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list TAKE]]
    }

    method compileDROP {addr args} {
        set next [expr {$addr + 1}]
        return [list [list $next $next] N N [list DROP]]
    }

}
