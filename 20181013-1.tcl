ADD  { my Store $a [expr {[lindex $data $b] + [lindex $data $c]}] }
CL   { if {$a eq {}} { set stack [linsert $stack 0 0] } else { lset stack $a 0 } }
CL   { lset registers $a [lindex $registers 0] }
CP   { lset data $a [lindex $data $b] }
D    { lset data $a [expr {[lindex $data $a] - 1}] }
DROP {}
DUP  { set stack [linsert $stack 0 [lindex $stack $a]] }
EQ   { my Store $a [expr {[lindex $data $b] eq [lindex $data $c]}] }
EQL  { my Store $a [expr {[lindex $data $b] == [lindex $data $c]}] }
G    { set returns [linsert $returns 0 [my vsets succ Q $ipointer]] }
H    { return -code continue }
H    { return -level 2 }
I    { lset data $a [expr {[lindex $data $a] + 1}] }
MOVE { lassign [my _Move $xpos $ypos $facing] xpos ypos }
MUL  { my Store $a [expr {[lindex $data $b] * [lindex $data $c]}] }
N    {}
PUSH { my Store 0 $a }
R    { set returns [lassign $returns next] }
T    { ... }
TAKE {}
TURN { set facing [my Turn $facing left] }

arithmetic as three-address: a ← b op c

method Store {addr value} {
    if {$addr < 0} {
        return -code error [format {illegal storage address %s} $addr]
    }
    # for CM
    lset registers $addr $value
    # for SM: addr = 0 pushes a new value
    set stack [lreplace $stack 0 $addr-1 $value]
}

proc Store {addr value} {
    global stack
    if {$addr < 0} {
        return -code error [format {illegal storage address %s} $addr]
    }
    # for SM: addr = 0 pushes a new value
    set stack [lreplace $stack 0 $addr-1 $value]
}

