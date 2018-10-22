DROP {}
G    { set returns [linsert $returns 0 [my vsets succ Q $ipointer]] }
H    { return -code continue }
H    { return -level 2 }
MOVE { lassign [my _Move $xpos $ypos $facing] xpos ypos }
N    {}
R    { set returns [lassign $returns next] }
T    { ... }
TAKE {}
TURN { set facing [my Turn $facing left] }

ADD  { set data [my Store $data $a [expr {[lindex $data $b] + [lindex $data $c]}]] }
CL   { set data [my Store $data $a 0] }
CP   { set data [my Store $data $a [lindex $data $b]] }
D    { set data [my Store $data $a [expr {[lindex $data $a] - 1}]] }
DUP  { set data [my Store $data -1 [lindex $data 0]] }
EQ   { set data [my Store $data $a [expr {[lindex $data $b] eq [lindex $data $c]}]] }
EQL  { set data [my Store $data $a [expr {[lindex $data $b] == [lindex $data $c]}]] }
I    { set data [my Store $data $a [expr {[lindex $data $a] + 1}]] }
MUL  { set data [my Store $data $a [expr {[lindex $data $b] * [lindex $data $c]}]] }
PUSH { set data [my Store $data -1 $a] }

arithmetic as three-address: a ← b op c

method Store {addr value} {
    # for CM
    lset registers $addr $value
    # for SM: addr = 0 pushes a new value
    set stack [lreplace $stack 0 $addr-1 $value]
}

proc Store {x addr value} {
    global stack
    # for CM: x ← {}
    if {$x eq {}} { set x $addr }
    # for SM: x ← -1
    # for SM: addr = -1 pushes a new value
    set stack [lreplace $stack $x $addr $value]
}

