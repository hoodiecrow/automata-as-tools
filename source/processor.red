
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

execute: func [
		op
		args [block!]
	][
	;-- TODO 
	jmp: first args
	ret/(:p/r): 1 + p/i
	switch model [
		"CM" [
			if-unary op [ p/a: first args ]
			if-binary op [
		        p/a: first args
		        p/b: second args
		        p/c: third args
			]
	        if-const op [
		        p/a: second args
			]
	        if 'CLEAR == op [
	        	p/a: first args
	        ]
	        if-copy op [
	        	p/a: first args
	        	p/b: second args
	        ]
	        if 'CMP == op [
		        p/a: first args
		        p/b: second args
		        p/c: third args
	        ]
		]
		"SM" [
			if-unary op [ p/a: p/s ]
			if-binary op [
		        p/b: p/s
		        -- p/s
		        p/c: p/s
		        p/a: p/s
			]
	        if-const op [
	        	++ p/s
	        	p/a: p/s
			]
	        if 'CLEAR == op [
	        	p/a: p/s
	        ]
	        if-copy op [
	        	p/b: p/s
	        	++ p/s
	        	p/a: p/s
	        ]
	        if 'CMP == op [
		        p/b: p/s
		        p/c: p/s
		        -- p/c
	        ]
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
