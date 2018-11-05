
Red []

comment {

Code: <label> <op> <a> <b> <c>

Mach: <state> <input> <data…>

Cm: op a b c
SM: op -1 -0 -1

Read from #0 means literal 0
Write or jump to #0 is an error 
Address #0 can be used as a null pointer 

}

++: func ['val [word!]] [set val 1 + get val]
--: func ['val [word!]] [if 0 < get val [set val -1 + get val]]

reset: func [m][
	ap: bp: cp: ip: rp: sp: 0
	rp: 51
	mem: make vector! 60
	jmp: 0
	cmp: 0
	model: m
]

comment {
	; http://www.red-by-example.org/parse.html
    ws: charset reduce [space tab cr lf]
    digit:   charset "0123456789"
    letters: charset [#"a" - #"z" #"A" - #"Z"]
    chars: union union letters digit charset "_"
    parse "  a:  FOO:a,b,c  BAR:a,b,c" tokens: [collect [some [ws | [":" | ","] | keep some chars]]]
 ;== [#"a" "FOO" #"a" #"b" #"c" "BAR" #"a" #"b" #"c"]
    split "  a:  FOO:a,b,c  BAR:a,b,c" ws
    split "  a:  FOO:a,b,c  BAR:a,b,c" [some ws] ;-- ?
}

execute-code: func [ops [block!]][
]

execute: func [op args [block!]] [
	operation: copy/part (next find operations op) 6
	jmp: first args
	mem/:rp: 1 + ip
	set-pointers operation/a args
	ip: mem/:rp
	do operation/b
	if get operation/c [cmp: do-cmp]
]

set-pointers: func [type args [block!]][
	switch model [
		"CM" [
			switch type [
				noarg    []
				onearg   [ ap: first args ]
				onearg2  [ ap: second args ]
				copyarg  [
					ap: first args
					bp: second args
				]
				threearg [
					ap: first args
					bp: second args
					cp: third args
				]
				cmparg [
					ap: first args
					bp: second args
					cp: third args
				]
				litarg   [
				]
			]
		]
		"SM" [
			switch type [
				noarg    []
				onearg   [ ap: sp ]
				onearg2  [ ++ sp ap: sp ]
				copyarg  [
					bp: sp
					++ sp
					ap: sp
				]
				threearg [
					bp: sp
					-- sp
					cp: sp
					ap: sp
				]
				cmparg [
					bp: sp
					cp: sp
					-- cp
				]
				litarg   [
				]
			]
		]
	]
]

operations: [
	PUSH   a noarg     b [++ sp mem/:sp: jmp]                   c no
	POP    a noarg     b [-- sp]                                c no
	CMP    a cmparg    b [cmp: either mem/:bp < mem/:cp [-1][either mem/:bp > mem/:cp [1][0]]] c no
	CLEAR  a onearg    b [mem/:ap: 0]                           c yes
	COPY   a copyarg   b [mem/:ap: mem/:bp]                     c yes
	DUP    a copyarg   b [mem/:ap: mem/:bp]                     c yes
	NOT    a onearg    b [mem/:ap: either mem/:ap == 0 [1][0]]  c yes
    NEG    a onearg    b [mem/:ap: - mem/:ap]                   c yes
    ABS    a onearg    b [mem/:ap: absolute mem/:ap]            c yes
	INC    a onearg    b [mem/:ap: mem/:ap + 1]                 c yes
    DEC    a onearg    b [mem/:ap: mem/:ap - 1]                 c yes
    CONST  a onearg2   b [mem/:ap: jmp]                         c yes
    EQ     a threearg  b [mem/:ap: mem/:bp ==  mem/:cp]         c yes
    NE     a threearg  b [mem/:ap: mem/:bp <>  mem/:cp]         c yes
    EQL    a threearg  b [mem/:ap: mem/:bp =   mem/:cp]         c yes
    NEQ    a threearg  b [mem/:ap: mem/:bp <>  mem/:cp]         c yes
    GT     a threearg  b [mem/:ap: mem/:bp >   mem/:cp]         c yes
    GE     a threearg  b [mem/:ap: mem/:bp >=  mem/:cp]         c yes
    LT     a threearg  b [mem/:ap: mem/:bp <   mem/:cp]         c yes
    LE     a threearg  b [mem/:ap: mem/:bp <=  mem/:cp]         c yes
    ADD    a threearg  b [mem/:ap: mem/:bp +   mem/:cp]         c yes
    SUB    a threearg  b [mem/:ap: mem/:bp -   mem/:cp]         c yes
    MUL    a threearg  b [mem/:ap: mem/:bp *   mem/:cp]         c yes
    EXP    a threearg  b [mem/:ap: mem/:bp **  mem/:cp]         c yes
    DIV    a threearg  b [mem/:ap: mem/:bp /   mem/:cp]         c yes
    MOD    a threearg  b [mem/:ap: mem/:bp %   mem/:cp]         c yes
    AND    a threearg  b [mem/:ap: mem/:bp and mem/:cp]         c yes
    OR     a threearg  b [mem/:ap: mem/:bp or  mem/:cp]         c yes
    XOR    a threearg  b [mem/:ap: mem/:bp xor mem/:cp]         c yes
    JUMP   a noarg     b [ip: jmp]                              c no
    JEQ    a noarg     b [if cmp == 0 [ip: jmp]]                c no
    JNE    a noarg     b [if cmp <> 0 [ip: jmp]]                c no
    JGT    a noarg     b [if cmp >  0 [ip: jmp]]                c no
    JGE    a noarg     b [if cmp >= 0 [ip: jmp]]                c no
    J0     a noarg     b [if cmp == 0 [ip: jmp]]                c no
    J1     a noarg     b [if cmp == 1 [ip: jmp]]                c no
    JZ     a noarg     b [if cmp == 0 [ip: jmp]]                c no
    JNZ    a noarg     b [if cmp <> 0 [ip: jmp]]                c no
    CALL   a noarg     b [++ rp ip: jmp]                        c no
    RET    a noarg	   b [-- rp ip: mem/:rp]                    c no 
    NOP    a noarg     b []                                     c no
    HALT   a noarg     b ;stop processor
    OUT    a litarg
    TEST   a litarg
]

do-cmp: does [either mem/:ap < 0 [-1][either mem/:ap > 0 [1][0]]]
