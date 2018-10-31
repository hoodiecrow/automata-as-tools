
Red []

execute: func [op a b c][
	;-- TODO 
	p/j: a
	mem/(:p/r): 1 + p/i
	either false [][
		p/i: mem/(:p/r)
	]
]

p: make map! [
	a 0
	b 0
	c 0
	i 0
	j 0
	r 1
	s 1
]

mem: make vector! 50

++: func ['val [path!]] [set val 1 + get val]
--: func ['val [path!]] [if 0 < get val [set val -1 + get val]]

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
