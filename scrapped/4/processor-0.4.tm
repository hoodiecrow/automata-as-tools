

namespace eval automata {}

oo::class create ::automata::Processor {
    method flagN {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagG {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagR {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagJC {id code} {
        lassign $code tag a b
        dict with id {
            set flag [expr {[my Access $a] eq [my Access $b]}]
        }
        return $id
    }

    method flagH {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagTEST {id code} {
        lassign $code tag a
        dict with id {
            set flag [my FlagTest $a]
        }
    }

    method flagI {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagD {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagCL {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagCP {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagPUSH {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagDUP {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagEQ {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagEQL {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagADD {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagMUL {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagTURN {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagMOVE {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method flagTAKE {id code} {
        dict with $id { set flag 0 }
        return $id
    }

    method flagDROP {id code} {
        dict with $id { set flag 0 }
        return $id
    }

    method execN {id code} {
        return $id
    }

    method execG {id code} {
        dict with id {
            set returns [linsert $returns 0 [my vsets succ Q $ipointer]]
        }
        return $id
    }

    method execR {id code} {
        dict with id {
            set returns [lassign $returns next]
            set ipointer $next
        }
        return $id
    }

    method execJC {id code} {
        dict with id { set flag 0 }
        return $id
    }

    method execH {id code} {
        return -level 2
    }

    method execTEST {id code} {
        return $id
    }

    method execI {id code} {
        lassign $code a
        dict with id {
            my Access $a [expr {[my Access $a] + 1}]
        }
        return $id
    }

    method execD {id code} {
        lassign $code a
        dict with id {
            my Access $a [expr {[my Access $a] - 1}]
        }
        return $id
    }

    method execCL {id code} {
        lassign $code a
        dict with id {
            my Access $a 0
        }
        return $id
    }

    method execCP {id code} {
        lassign $code a b
        dict with id {
            my Access $a [my Access $b]
        }
        return $id
    }

    method execPUSH {id code} {
        lassign $code a
        dict with id {
            my Access {} $a
        }
        return $id
    }

    method execDUP {id code} {
        dict with id {
            my Access {} [my Access 0]
        }
        return $id
    }

    method execEQ {id code} {
        lassign $code a b
        dict with id {
            my Access {} [expr {[my Access $a] eq [my Access $b]}]
        }
        return $id
    }

    method execEQL {id code} {
        lassign $code a b
        dict with id {
            my Access {} [expr {[my Access $a] == [my Access $b]}]
        }
        return $id
    }

    method execADD {id code} {
        lassign $code a b
        dict with id {
            my Access {} [expr {[my Access $a] + [my Access $b]}]
        }
        return $id
    }

    method execMUL {id code} {
        lassign $code a b
        dict with id {
            my Access {} [expr {[my Access $a] * [my Access $b]}]
        }
        return $id
    }

    method execTURN {id code} {
        lassign $code a b
        dict with id {
            set facing [my Turn $facing left]
        }
        return $id
    }

    method execMOVE {id code} {
        lassign $code a b
        dict with id {
            lassign [my _Move $xpos $ypos $facing] xpos ypos
            if {[my CheckCollision $width $height $xpos $ypos $walls]} {
                return -code error [format {collision with a wall!}]
            }
        }
        return $id
    }

    method execTAKE {id code} {
        return $id
    }

    method execDROP {id code} {
        return $id
    }

}
