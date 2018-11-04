
Red []

reset: func [m][
	foreach key [ap bp cp ip rp sp] [
		set :key 0
	]
	++ rp
	mem: make vector! 50
	ret: make vector! 10
	jmp: 0
	cmp: 0
	model: m
]

execute: func [op args [block!]] [
	operation: copy/part (next find operations op) 6
	jmp: first args
	ret/:rp: 1 + ip
	set-pointers operation/a args
	ip: ret/:rp
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
    RET    a noarg	   b [-- rp ip: ret/:rp]                    c no 
    NOP    a noarg     b []                                     c no
    HALT   a noarg     b ;stop processor
    OUT    a litarg
    TEST   a litarg
]

do-cmp: does [either mem/:ap < 0 [-1][either mem/:ap > 0 [1][0]]]

++: func ['val [word!]] [set val 1 + get val]
--: func ['val [word!]] [if 0 < get val [set val -1 + get val]]

execute0: func [
		op
		args [block!]
	][
	;-- TODO 
	jmp: first args
	ret/:rp: 1 + ip
	switch model [
		"CM" [
			onearg: [ ap: first args ]
			onearg2: [ ap: second args ]
			twoarg: [
	        	ap: first args
	        	bp: second args
	        ]
			threearg: [
		        ap: first args
		        bp: second args
		        cp: third args
	        ]
			if-unary op onearg
	        if 'CLEAR == op onearg
	        if-const op onearg2
	        if-copy op twoarg
	        if 'CMP == op threearg
			if-binary op threearg
		]
		"SM" [
            onearg: [ ap: sp ]
            pusharg: [ ++ sp ap: sp ]
            pushtwoarg: [
            	bp: sp
            	++ sp
            	ap: sp
            ]
            threearg: [
            	bp: sp
            	-- sp
            	cp: sp
            	ap: sp
            ]
            cmparg: [
            	bp: sp
            	cp: sp
            	-- cp
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
		poke mem ap (switch op [
			NOT [either mem/:ap == 0 [1][0]]
			NEG [- mem/:ap]
			ABS [absolute mem/:ap]
			INC [mem/:ap + 1]
			DEC [mem/:ap - 1]
		])
		cmp: do-cmp
	]
	if-binary op [
		operator: get op
		poke mem ap operator mem/:bp mem/:cp
		cmp: do-cmp
	]
	if-const op [
		mem/:ap: jmp
		cmp: do-cmp
	]
	if 'CLEAR == op [
		poke mem ap 0
		cmp: do-cmp
	]
	if-copy op [
		poke mem ap mem/:bp
		cmp: do-cmp
	]
	if 'CMP == op [
		cmp: either mem/:bp < mem/:cp [
			-1
		][
		either mem/:bp > mem/:cp [
			1
		][
			0
		]]
	]
	ip: ret/:rp
	if-jump op [
		switch op [
			JUMP [ip: jmp]
			CALL [
				++ rp
				ip: jmp
			]
			RET [
				-- rp
				ip: ret/:rp
			]
			JEQ [ if cmp == 0 [ip: jmp] ]
			JNE [ if cmp <> 0 [ip: jmp] ]
			JGT [ if cmp > 0 [ip: jmp] ]
			JGE [ if cmp >= 0 [ip: jmp] ]
			J0 [ if cmp == 0 [ip: jmp] ]
			J1 [ if cmp == 1 [ip: jmp] ]
			JZ [ if cmp == 0 [ip: jmp] ]
			JNZ [ if cmp <> 0 [ip: jmp] ]
		]
	]
]

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
