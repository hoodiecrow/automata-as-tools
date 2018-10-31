
Red []

pointers: make map! [
	ap 0
	bp 0
	cp 0
	ip 0
	jp 0
	rp 0
	sp 0
]

memory: make vector! 50

resetptr: func [key][pointers/:key: 0]
copyptr: func [key1 key2][pointers/:key1: pointers/:key2]
incptr: func [key][pointers/:key: pointers/:key + 1]
decptr: func [key][if pointers/:key > 0 [pointers/:key: pointers/:key - 1]]

memset: func [key val][poke memory pointers/:key val]
memget: func [key][pick memory pointers/:key]

arch: [
	jmp: 0
	rp: 0
	ip: 0
	jexpr: FALSE
	model: ""
	ap: 0
	bp: 0
	cp: 0
	sp: 0
	regs: make vector! [0]
	rets: make vector! [0]
	stack: make vector! [0]

]

execute: func [op a b c][
	;-- TODO 
]

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
