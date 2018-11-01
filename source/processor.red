
Red []

p: make map! 7

reset: does [
	foreach key [a b c i j s] [
		p/(key): 0
	]
	foreach key [r] [
		p/(key): 1
	]
	mem: make vector! 50
	ret: make vector! 10
]

execute: func [op a b c][
	;-- TODO 
	p/j: a
	ret/(:p/r): 1 + p/i
	if-unary op [p/a: p/s]
	if-binary op [
		p/b: p/s
		-- p/s
		p/c: p/s
		p/a: p/s
		;use [operator][
		operator: get op
		poke mem p/a operator mem/(p/b) mem/(p/c)
	]
	if-const op [
		++ p/s
		mem/(:p/s): p/j
	]
	either false [][
		p/i: ret/(:p/r)
	]
]

++: func ['val [path!]] [set val 1 + get val]
--: func ['val [path!]] [if 0 < get val [set val -1 + get val]]

if-unary: func [op body][
	if none <> find [NOT NEG INC DEC] op [do body]
]

if-binary: func [op body][
	if none <> find [
        EQ NE EQL NEQ GT GE LT LE ADD SUB MUL DIV MOD AND OR XOR
	] op [do body]
]

if-const: func [op body][
	if none <> find [CONST] op [do body]
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
