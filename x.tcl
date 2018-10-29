
proc execute {op a b c} {
    global arch
    set arch(jmp) $a
    set arch(rp) [expr {$arch(ip) + 1}]
    if {$op in {NOT NEG INC DEC}} {
        set arch(op) $op
        set op UNARY
    }
    if {$op in {
        EQ NE EQL NEQ GT GE LT LE ADD SUB MUL DIV MOD AND OR XOR
    }} {
        set arch(op) $op
        set op BINARY
    }
    if {$op in {JUMP CALL RET}} {
        set arch(jexpr) TRUE
    } elseif {$op in {JEQ JNE JGT JGE J0 J1 JZ JNZ}} {
        set arch(jexpr) [string range $op 1 end]
    }
    if {$op eq "CALL"} { incr arch(rp) }
    if {$op eq "RET"} {
        set arch(jmp) $arch(rp)
        incr arch(rp) -1
    }
    switch $arch(model) {
        CM {
            set arch(ap) $a
            # set <=>
            set arch(bp) $b
            set arch(cp) $c
        }
        PTM {
            set arch(ap) ;# ...
            # set <=>
        }
        SM {
            set arch(bp) $arch(sp)
            set arch(cp) [expr {$arch(sp) - 1}]
            if {$op in {CONST DUP}} {incr arch(sp)}
            if {$op in {BINARY DROP}} {incr arch(sp) -1}
            set arch(ap) $arch(sp)
            # set <=>
        }
    }
    if {$op eq "CONST"} {lset mem $arch(ap) $a}
    if {$op eq "CLEAR"} {lset mem $arch(ap) 0}
    if {$op in {COPY DUP}} {lset mem $arch(ap) $arch(bp)}
    if {$op eq "UNARY"} {lset mem $arch(ap) [unary $arch(op)]}
    if {$op in "BINARY"} {lset mem $arch(ap) [binary $op]}
    if {$op eq "CMP"} $op
    if {[$arch(jexpr) $arch(<=>)]} {
        set arch(ip) $arch(jmp)
        set arch(jexpr) FALSE
    } else {
        set arch(ip) [lindex $mem $arch(rp)]
    }

}

