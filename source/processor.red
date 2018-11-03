
Red []

p: make map! 7

reset: func [m][
	foreach key [a b c i r s] [
		p/(key): 0
	]
	++ p/r
	mem: make vector! 50
	ret: make vector! 10
	jmp: 0
	cmp: 0
	model: m
]

operations: [
	PUSH   noarg     [++ p/s mem/(:p/s): jmp]                     no
	POP    noarg     [-- p/s]                                     no
	CLEAR  onearg    [mem/(:p/a): 0]                              yes
	COPY   copyarg   [mem/(:p/a): mem/(:p/b)]                     yes
	DUP    copyarg   [mem/(:p/a): mem/(:p/b)]                     yes
	NOT    onearg    [mem/(:p/a): either mem/(:p/a) == 0 [1][0]]  yes
    NEG    onearg    [mem/(:p/a): - mem/(:p/a)]                   yes
    ABS    onearg    [mem/(:p/a): absolute mem/(:p/a)]            yes
	INC    onearg    [mem/(:p/a): mem/(:p/a) + 1]                 yes
    DEC    onearg    [mem/(:p/a): mem/(:p/a) - 1]                 yes
    CONST  onearg2   [mem/(:p/a): jmp]                            yes
    EQ     threearg  [mem/(:p/a): mem/(:p/b) ==  mem/(:p/c)]      yes
    NE     threearg  [mem/(:p/a): mem/(:p/b) <>  mem/(:p/c)]      yes
    EQL    threearg  [mem/(:p/a): mem/(:p/b) =   mem/(:p/c)]      yes
    NEQ    threearg  [mem/(:p/a): mem/(:p/b) <>  mem/(:p/c)]      yes
    GT     threearg  [mem/(:p/a): mem/(:p/b) >   mem/(:p/c)]      yes
    GE     threearg  [mem/(:p/a): mem/(:p/b) >=  mem/(:p/c)]      yes
    LT     threearg  [mem/(:p/a): mem/(:p/b) <   mem/(:p/c)]      yes
    LE     threearg  [mem/(:p/a): mem/(:p/b) <=  mem/(:p/c)]      yes
    ADD    threearg  [mem/(:p/a): mem/(:p/b) +   mem/(:p/c)]      yes
    SUB    threearg  [mem/(:p/a): mem/(:p/b) -   mem/(:p/c)]      yes
    MUL    threearg  [mem/(:p/a): mem/(:p/b) *   mem/(:p/c)]      yes
    EXP    threearg  [mem/(:p/a): mem/(:p/b) **  mem/(:p/c)]      yes
    DIV    threearg  [mem/(:p/a): mem/(:p/b) /   mem/(:p/c)]      yes
    MOD    threearg  [mem/(:p/a): mem/(:p/b) %   mem/(:p/c)]      yes
    AND    threearg  [mem/(:p/a): mem/(:p/b) and mem/(:p/c)]      yes
    OR     threearg  [mem/(:p/a): mem/(:p/b) or  mem/(:p/c)]      yes
    XOR    threearg  [mem/(:p/a): mem/(:p/b) xor mem/(:p/c)]      yes
	; p/i: ret/(:p/r)
    JUMP   noarg     [p/i: jmp]                                   no
    JEQ    noarg     [if cmp == 0 [p/i: jmp]]                     no
    JNE    noarg     [if cmp <> 0 [p/i: jmp]]                     no
    JGT    noarg     [if cmp >  0 [p/i: jmp]]                     no
    JGE    noarg     [if cmp >= 0 [p/i: jmp]]                     no
    J0     noarg     [if cmp == 0 [p/i: jmp]]                     no
    J1     noarg     [if cmp == 1 [p/i: jmp]]                     no
    JZ     noarg     [if cmp == 0 [p/i: jmp]]                     no
    JNZ    noarg     [if cmp <> 0 [p/i: jmp]]                     no
    CALL   noarg     [++ p/r p/i: jmp]                            no
    RET    noarg	 [-- p/r p/i: ret/(:p/r)]                     no
    NOP    noarg     []                                           no
    HALT   noarg     ;stop processor
    OUT    litarg
    TEST   litarg
]

execute: func [
		op
		args [block!]
	][
	;-- TODO 
	jmp: first args
	ret/(:p/r): 1 + p/i
	switch model [
		"CM" [
			onearg: [ p/a: first args ]
			onearg2: [ p/a: second args ]
			twoarg: [
	        	p/a: first args
	        	p/b: second args
	        ]
			threearg: [
		        p/a: first args
		        p/b: second args
		        p/c: third args
	        ]
			if-unary op onearg
	        if 'CLEAR == op onearg
	        if-const op onearg2
	        if-copy op twoarg
	        if 'CMP == op threearg
			if-binary op threearg
		]
		"SM" [
            onearg: [ p/a: p/s ]
            pusharg: [ ++ p/s p/a: p/s ]
            pushtwoarg: [
            	p/b: p/s
            	++ p/s
            	p/a: p/s
            ]
            threearg: [
            	p/b: p/s
            	-- p/s
            	p/c: p/s
            	p/a: p/s
            ]
            cmparg: [
            	p/b: p/s
            	p/c: p/s
            	-- p/c
            ]
			if-unary op onearg
	        if 'CLEAR == op onearg
	        if-const op pusharg
	        if-copy op pushtwoarg
	        if 'CMP == op cmparg
			if-binary op threearg
		]
	]
	if-unary op [
		poke mem p/a (switch op [
			NOT [either mem/(:p/a) == 0 [1][0]]
			NEG [- mem/(:p/a)]
			ABS [absolute mem/(:p/a)]
			INC [mem/(:p/a) + 1]
			DEC [mem/(:p/a) - 1]
		])
		cmp: do-cmp
	]
	if-binary op [
		operator: get op
		poke mem p/a operator mem/(:p/b) mem/(:p/c)
		cmp: do-cmp
	]
	if-const op [
		mem/(:p/a): jmp
		cmp: do-cmp
	]
	if 'CLEAR == op [
		poke mem p/a 0
		cmp: do-cmp
	]
	if-copy op [
		poke mem p/a mem/(:p/b)
		cmp: do-cmp
	]
	if 'CMP == op [
		cmp: either mem/(:p/b) < mem/(:p/c) [
			-1
		][
		either mem/(:p/b) > mem/(:p/c) [
			1
		][
			0
		]]
	]
	p/i: ret/(:p/r)
	if-jump op [
		switch op [
			JUMP [p/i: jmp]
			CALL [
				++ p/r
				p/i: jmp
			]
			RET [
				-- p/r
				p/i: ret/(:p/r)
			]
			JEQ [ if cmp == 0 [p/i: jmp] ]
			JNE [ if cmp <> 0 [p/i: jmp] ]
			JGT [ if cmp > 0 [p/i: jmp] ]
			JGE [ if cmp >= 0 [p/i: jmp] ]
			J0 [ if cmp == 0 [p/i: jmp] ]
			J1 [ if cmp == 1 [p/i: jmp] ]
			JZ [ if cmp == 0 [p/i: jmp] ]
			JNZ [ if cmp <> 0 [p/i: jmp] ]
		]
	]
]

do-cmp: does [either mem/(:p/a) < 0 [-1][either mem/(:p/a) > 0 [1][0]]]

++: func ['val [path!]] [set val 1 + get val]
--: func ['val [path!]] [if 0 < get val [set val -1 + get val]]

if-unary: func [op body][
	if none <> find [NOT NEG ABS INC DEC] op [do body]
]

if-binary: func [op body][
	if none <> find [
        EQ NE EQL NEQ GT GE LT LE ADD SUB MUL DIV MOD AND OR XOR
	] op [do body]
]

if-const: func [op body][
	if none <> find [CONST] op [do body]
]

if-copy: func [op body][
	if none <> find [COPY DUP] op [do body]
]

if-jump: func [op body][
	if none <> find [JUMP CALL RET JEQ JNE JGT JGE J0 J1 JZ JNZ] op [do body]
]

comment {
resetptr: func [key][pointers/:key: 0]
			  setptr: func [key val][pointers/:key: val]
			  copyptr: func [key1 key2][pointers/:key1: pointers/:key2]
			  incptr: func [key][pointers/:key: pointers/:key + 1]
			  decptr: func [key][if pointers/:key > 0 [pointers/:key: pointers/:key - 1]]
}

memset: func [key val][poke memory p/:key val]
memget: func [key][pick memory p/:key]

ops: make map! [
	;--    type   jmp      jexpr      body
	'NOT  [UNARY  none     FALSE      [unop  not ap]]
	'EQ   [BINARY none     FALSE      [binop bp == cp]]
	'EQL  [BINARY none     FALSE      [binop bp = cp]]
	'JUMP [JUMP   none     TRUE       []]
	'CALL [JUMP   none     TRUE       [rp: rp + 1]]
	'RET  [JUMP   rets/:rp TRUE       [rp: rp - 1]]
	'JEQ  [JCOND  none     [cmp == 0] []]
]
