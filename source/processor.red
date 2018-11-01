
Red []

p: make map! 7

reset: does [
	foreach key [a b c i j r s] [
		p/(key): 0
	]
	++ p/r
	mem: make vector! 50
	ret: make vector! 10
	cmp: 0
]

execute: func [op a b c][
	;-- TODO 
	p/j: a
	ret/(:p/r): 1 + p/i
	if-unary op [
		p/a: p/s
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
		p/b: p/s
		-- p/s
		p/c: p/s
		p/a: p/s
		operator: get op
		poke mem p/a operator mem/(:p/b) mem/(:p/c)
		cmp: do-cmp
	]
	if-const op [
		++ p/s
		p/a: p/s
		mem/(:p/a): p/j
		cmp: do-cmp
	]
	if 'CLEAR == op [
		p/a: p/s
		poke mem p/a 0
		cmp: do-cmp
	]
	if-copy op [
		p/b: p/s
		++ p/s
		p/a: p/s
		poke mem p/a mem/(:p/b)
		cmp: do-cmp
	]
	if 'CMP == op [
		p/b: p/s
		p/c: p/s
		-- p/c
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
			JUMP [p/i: p/j]
			CALL [
				++ p/r
				p/i: p/j
			]
			RET [
				-- p/r
				p/i: ret/(:p/r)
			]
			JEQ [ if cmp == 0 [p/i: p/j] ]
			JNE [ if cmp <> 0 [p/i: p/j] ]
			JGT [ if cmp > 0 [p/i: p/j] ]
			JGE [ if cmp >= 0 [p/i: p/j] ]
			J0 [ if cmp == 0 [p/i: p/j] ]
			J1 [ if cmp == 1 [p/i: p/j] ]
			JZ [ if cmp == 0 [p/i: p/j] ]
			JNZ [ if cmp <> 0 [p/i: p/j] ]
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
