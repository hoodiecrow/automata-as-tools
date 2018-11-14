
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
	zflag: 0
	mem: make vector! 60
	_head: tape: [ 0 ]
	jmp: 0
	cmp: 0
	running: false
	model: m
	om: none
]

comment {
LDC  s              e  (LDC x.c)      d → (x.s)       e       c   d
OP   (a b.s)        e  (OP.c)         d → (b op a.s)  e       c   d
HALT s              e  (HALT)         d → s           e       NIL d
LD   s              e  (LD (m.n).c)   d → (x.s)       e       c   d
LDF  s              e  (LDF c'.c)     d → ((c'.e).s)  e       c   d
AP   ((c'.e') v.s)  e  (AP.c)         d → NIL         (v.e')  c'  (s e c.d)
RET  (x)            e' (RET)  (s e c.d) → (x.s)       e       c   d
NIL  s              e  (NIL.c)        d → (NIL.s)     e       c   d
CONS (a b.s)        e  (CONS.c)       d → ((a.b).s)   e       c   d
CAR  ((a.b).s)      e  (CAR.c)        d → (a.s)       e       c   d
CDR  ((a.b).s)      e  (CDR.c)        d → (b.s)       e       c   d
JOIN  s             e  (JOIN)     (c.d) → s           e       c   d
SEL   (x.s)         e  (SEL ct cf.c)  d → s           e       cx  (c.d)
NULL  s             e  (NULL.c)       d → (x.s)       e       c   d
PRINT
READ
(DUM)
(RAP)
 
INC  ldc 1, add
DEC  ldc 1, sub
CLR  dup, sub
CPY  decSP, dup
NEG  ldc -1, mul
CALL incRP, ip←jmp
RET  decRP, ip←[rp]
}

parse-instruction: does [
	word: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"_"]
	non-word: complement word
print ['parse-instruction mold ip mold program/:ip]
	instr: program/:ip
	if none? instr [cause-error 'foo 'bar ["off the end"]]
	if (not string? instr) [set 'instr to-string instr]
	parse instr [collect [some [keep some word | some non-word]]]
]

parse-argument: func [
	arg
	/address
][
	if none? arg [return 0]
	word: charset [#"a" - #"z" #"A" - #"Z" #"0" - #"9" #"_"]
	digit: charset [#"0" - #"9"]
	digits: [some digit]
	;-- TODO depending on which argument it is, an offset could be added to ip,
	;-- sp, etc
	either address [
		parse to-string arg [
			v: digits (res: to-integer v) |
			v: [["-" - "+"] digits] (res: ip + to-integer v) |
			v: "head" (res: 0 om: 'as-head) |
			v: some word (res: select labels to-word v)
		]
		if res [return res]
	][
		parse to-string arg [
			v: digits (res: to-integer v) |
			v: [["-" - "+"] digits] (res: to-string v) |
			v: some word (res: to-word v)
		]
		if res [return res]
	]
]

execute: func [
	init [block!]
	code [block!]
][
	do init
	set 'labels make map! []
	set 'program make block! length? code
    parse code [
		some [
			label: set-word! (put labels first label (length? program) + 1) |
			token: any-type! (append program first token)
		]
	]
	set 'ip 1
	set 'running true
	while [running][
		execute-instruction parse-instruction
	]
]

execute-instruction: func [instruction [series!]] [
print ['execute-instruction mold instruction]
;print mold find/skip operations (make lit-word! op) 7
	op: first instruction
	set 'a parse-argument/address second instruction
	set 'b parse-argument third instruction
	set 'c parse-argument fourth instruction
	set 'jmp a
	operation: copy/part (next find/skip operations make lit-word! op 7) 6
	mset rp (1 + ip)
	set-pointers operation/a reduce [a b c]
	set 'ip mem/:rp
;print ['ap ap 'bp bp 'cp cp 'ip ip 'rp rp 'sp sp 'jmp jmp 'cmp cmp 'mem8 mold copy/part mem 8]
	do operation/b
print ['ap ap 'bp bp 'cp cp 'ip ip 'rp rp 'sp sp 'mem8 mold copy/part mem 8]
	if get operation/c [set-cmp]
]

set-pointers: func [type args [block!]][
print ['set-pointers mold args]
	switch model [
		"CM" [
			switch type [
				noarg    []
				onearg   [ set 'ap first args ]
				onearg2  [ set 'ap second args ]
				copyarg  [ set [ap bp] args ]
				threearg [ set [ap bp cp] args ]
				cmparg   [ set [ap bp cp] args ]
				znzarg   [ set [ap bp cp] args ]
				litarg   [ ]
			]
		]
		"PTM" [
			switch type [
				noarg    []
				onearg   [ set 'ap first args ]
				onearg2  [ set 'ap second args ]
				copyarg  [ set [ap bp] args ]
				threearg [ set [ap bp cp] args ]
				cmparg   [ set [ap bp cp] args ]
				znzarg   [ set [ap bp cp] args ]
				litarg   [ set [ap bp cp] args ]
			]
		]
		"SM" [
			switch type [
				noarg    []
				onearg   [ set 'ap sp ]
				onearg2  [ ++ sp set 'ap sp ]
				copyarg  [
					set 'bp sp
					++ sp
					set 'ap sp
				]
				threearg [
					set 'bp sp
					-- sp
					set 'cp sp
					set 'ap sp
				]
				cmparg [
					set 'bp sp
					set 'cp sp
					-- cp
				]
				znzarg   [ set 'bp sp ]
				litarg   [
				]
			]
		]
	]
]

operations: [
	PUSH   a noarg     b [++ sp mset sp jmp]                   c no
	POP    a noarg     b [-- sp]                                c no
	CMP    a cmparg    b [do-cmp]                             c no
	CLEAR  a onearg    b [mset ap 0]                           c yes
	COPY   a copyarg   b [mset ap (mget bp)]                     c yes
	DUP    a copyarg   b [mset ap (mget bp)]                     c yes
	NOT    a onearg    b [mset ap either (mget ap) == 0 [1][0]]  c yes
    NEG    a onearg    b [mset ap - (mget ap)]                   c yes
    ABS    a onearg    b [mset ap absolute (mget ap)]            c yes
	INC    a onearg    b [mset ap (mget ap) + 1]                 c yes
    DEC    a onearg    b [mset ap (mget ap) - 1]                 c yes
    CONST  a onearg2   b [mset ap jmp]                      c yes
    EQ     a threearg  b [mset ap (mget bp) ==  (mget cp)]         c yes
    NE     a threearg  b [mset ap (mget bp) <>  (mget cp)]         c yes
    EQL    a threearg  b [mset ap (mget bp) =   (mget cp)]         c yes
    NEQ    a threearg  b [mset ap (mget bp) <>  (mget cp)]         c yes
    GT     a threearg  b [mset ap (mget bp) >   (mget cp)]         c yes
    GE     a threearg  b [mset ap (mget bp) >=  (mget cp)]         c yes
    LT     a threearg  b [mset ap (mget bp) <   (mget cp)]         c yes
    LE     a threearg  b [mset ap (mget bp) <=  (mget cp)]         c yes
    ADD    a threearg  b [mset ap (mget bp) +   (mget cp)]         c yes
    SUB    a threearg  b [mset ap (mget bp) -   (mget cp)]         c yes
    MUL    a threearg  b [mset ap (mget bp) *   (mget cp)]         c yes
    EXP    a threearg  b [mset ap (mget bp) **  (mget cp)]         c yes
    DIV    a threearg  b [mset ap (mget bp) /   (mget cp)]         c yes
    MOD    a threearg  b [mset ap (mget bp) %   (mget cp)]         c yes
    AND    a threearg  b [mset ap (mget bp) and (mget cp)]         c yes
    OR     a threearg  b [mset ap (mget bp) or  (mget cp)]         c yes
    XOR    a threearg  b [mset ap (mget bp) xor (mget cp)]         c yes
    JUMP   a noarg     b [set 'ip jmp]                          c no
    JEQ    a cmparg    b [if do-cmp == 0 [set 'ip jmp]]                c no
    JNE    a cmparg    b [if do-cmp <> 0 [set 'ip jmp]]                c no
    JGT    a cmparg    b [if do-cmp >  0 [set 'ip jmp]]                c no
    JGE    a cmparg    b [if do-cmp >= 0 [set 'ip jmp]]                c no
    J0     a noarg     b [if cmp == 0 [set 'ip jmp]]                c no
    J1     a noarg     b [if cmp == 1 [set 'ip jmp]]                c no
    JZ     a znzarg    b [if (mget bp) == 0 [set 'ip jmp]]                c no
    JNZ    a znzarg    b [if (mget bp) <> 0 [set 'ip jmp]]                c no
    CALL   a noarg     b [++ rp set 'ip jmp]                        c no
    RET    a noarg	   b [-- rp set 'ip (mget rp)]                    c no 
    NOP    a noarg     b []                                     c no
    HALT   a noarg     b [set 'running false]                       c no
    OUT    a litarg    b [do-out om bp cp set 'running false]                         c no
    TEST   a litarg
]

; in SM, #0 should be ToS?
mset: func [index [any-type!] value][if index > 0 [poke mem index value] ]
mget: func [index [integer!]][ either index > 0 [pick mem index][return 0] ]
set-cmp: does [
	set 'cmp either (mget ap) < 0 [-1][either (mget ap) > 0 [1][0]]
	set 'zflag (mget ap) == 0
]
do-cmp: does [cmp: either (mget bp) < (mget cp) [-1][either (mget bp) > (mget cp) [1][0]]]

do-out: func [ aarg barg carg ][
print ['do-out mold aarg type? aarg mold barg type? barg mold carg type? carg]
print [mold _head]
	if false [
		set 'running false
			return none
	]
	switch aarg [
		as-head [
			switch barg [
				P [change _head 1 ]
				E [change _head 0 ]
			]
		]
	]
]

comment {
		set 'running false
			return none
			; head moves over tape
			switch carg [
				R [
					next _head
					if tail? _head [
						insert _head 0
					]
				]
				L [
					back _head
					if head? _head [
						insert _head 0
					]
				]
			]
		set 'running false
			return none
}
