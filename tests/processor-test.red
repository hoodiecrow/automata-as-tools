Red [
  Title:   "Testing processor.red"
  File:    %processor-test.red
]

do %/C/users/peter/code/red/quick-test/quick-test.red
#include append what-dir %../source/processor.red

~~~start-file~~~ "processor"


===start-group=== "stack machine"


	reset "SM"
  --test-- "stack-machine-test-1" --assert 0 = ap

  --test-- "stack-machine-test-2" 
	reset "SM"
	++ ap
  --assert 1 = ap


  --test-- "stack-machine-test-3" 
	reset "SM"
	-- ap
	-- ap
	-- ap
  --assert 0 = ap

  --test-- "stack-machine-test-4" 
	reset "SM"
	++ ap
	mem/(:ap): 3
  --assert 3 = mem/1

  --test-- "stack-machine-test-5"
	reset "SM"
	++ ap
	mem/(:ap): 2
  --assert 2 = mem/(:ap)

  --test-- "stack-machine-test-6"
	reset "SM"
    sp: 0
	++ sp mem/(:sp): 3
	++ sp mem/(:sp): 2
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-7"
	reset "SM"
    sp: 0
	++ sp mem/(:sp): 3
	++ sp mem/(:sp): 2
	bp: sp
	-- sp
	cp: sp
  --assert [2 3] = append append [] mem/(:bp) mem/(:cp)

  --test-- "stack-machine-test-8"
	reset "SM"
    sp: 0
	++ sp mem/(:sp): 3
	++ sp mem/(:sp): 2
	bp: sp
	-- sp
	cp: sp
	ap: sp
	mem/(:ap): ADD mem/(:bp) mem/(:cp)
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-9"
	reset "SM"
    sp: 0
	++ sp mem/(:sp): 4
	++ sp mem/(:sp): 3
	++ sp mem/(:sp): 2
	bp: sp
	-- sp
	cp: sp
	ap: sp
	mem/(:ap): ADD mem/(:bp) mem/(:cp)
	bp: sp
	-- sp
	cp: sp
	ap: sp
	mem/(:ap): MULTIPLY mem/(:bp) mem/(:cp)
  --assert "make vector! [20 5 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-10"
	reset "SM"
    execute ["NOP" ]
  --assert 1 = ip

  --test-- "stack-machine-test-11"
	reset "SM"
    execute ["NOP" ]
    execute ["NOP" ]
  --assert 2 = ip

  --test-- "stack-machine-test-12"
	reset "SM"
    sp: 9
    execute ["NOT" 0]
  --assert 9 = ap

  --test-- "stack-machine-test-13"
	reset "SM"
    execute ["CONST" 3]
  --assert "make vector! [3 0 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-14"
	reset "SM"
    execute ["CONST" 3]
    execute ["CONST" 2]
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-15"
	reset "SM"
	jmp: 3 execute ["CONST" 3]
	jmp: 2 execute ["CONST" 2]
	jmp: 0 execute ["ADD" ]
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-16"
	reset "SM"
	jmp: 3 execute ["CONST" 3]
    jmp: 0 execute ["CLEAR" ]
	jmp: 2 execute ["CONST" 2]
    jmp: 0 execute ["DUP" ]
  --assert "make vector! [0 2 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-17"
	reset "SM"
	jmp: 3 execute ["CONST" 3]
	jmp: 2 execute ["CONST" 2]
    jmp: 0 execute ["CMP" ]
  --assert -1 = cmp

  --test-- "stack-machine-test-18"
	reset "SM"
	jmp: 2 execute ["CONST" 2]
	jmp: 3 execute ["CONST" 3]
    jmp: 0 execute ["CMP" ]
  --assert 1 = cmp

===end-group===

===start-group=== "counter machine"


  --test-- "counter-machine-test-6"
	reset "CM"
	cp: 1
	bp: 2
	mem/(:cp): 3
    mem/(:bp): 2
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-8"
	reset "CM"
	cp: 1
	bp: 2
	ap: 3
	mem/(:cp): 3
    mem/(:bp): 2
	mem/(:ap): ADD mem/(:bp) mem/(:cp)
  --assert "make vector! [3 2 5 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-9"
    reset "CM"
	insert mem [4 3 2]
	mem/4: ADD mem/3 mem/2
	mem/4: MULTIPLY mem/4 mem/1
  --assert "make vector! [4 3 2 20 0 0 0 0]" = mold copy/part mem 8 

  --test-- "counter-machine-test-10"
	reset "CM"
	execute ["NOP" ]
  --assert 1 = ip

  --test-- "counter-machine-test-11"
	reset "CM"
	execute ["NOP" ]
	execute ["NOP" ]
  --assert 2 = ip

  --test-- "counter-machine-test-12"
	reset "CM"
	mem/1: 1
	jmp: 1 execute ["NOT" 1]
  --assert 0 = mem/(:ap)

  --test-- "counter-machine-test-13"
	reset "CM"
	jmp: 3 execute ["CONST" 3 1]
  --assert "make vector! [3 0 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-14"
	reset "CM"
	execute ["CONST" 3 1]
	execute ["CONST" 2 2]
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-15"
	reset "CM"
	execute ["CONST" 3 1]
	execute ["CONST" 2 2]
	execute ["ADD" 3 2 1]
  --assert "make vector! [3 2 5 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-16"
	reset "CM"
	execute ["CONST" 3 1]
	execute ["CLEAR" 1]
	execute ["CONST" 2 2]
	execute ["COPY" 3 2]
  --assert "make vector! [0 2 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-17"
	reset "CM"
	execute ["CONST" 3 1]
	execute ["CONST" 2 2]
	execute ["CMP" 3 2 1]
  --assert -1 = cmp

  --test-- "counter-machine-test-18"
	reset "CM"
	execute-code [
		CONST:2,1
		CONST:3,2
		CMP:3,2,1
		HALT
	]
  --assert 1 = cmp

  --test-- "parse-argument-1"
    reset "CM"
	labels: [a: 1]
	instruction: [FOO #"a" #"2"]
	set 'res parse-argument second instruction
  --assert 1 = res

  --test-- "parse-argument-2"
    reset "CM"
	labels: [a: 1]
	instruction: [FOO #"a" #"2"]
	set 'res parse-argument third instruction
  --assert 2 = res

  --test-- "parse-argument-3"
    reset "CM"
	labels: [a: 1]
	instruction: [FOO #"a" #"2"]
	set 'res parse-argument fourth instruction
  --assert 0 = res

  --test-- "parse-argument-4"
    reset "CM"
	instruction: [FOO #"3" #"2"]
	set 'res parse-argument second instruction
  --assert 3 = res

  --test-- "parse-argument-5"
    reset "CM"
	instruction: [CONST #"3" #"2"]
	set 'a parse-argument second instruction
	set 'b parse-argument third instruction
	set 'c parse-argument fourth instruction
  --assert "[3 2 0]" = mold reduce [a b c]

===end-group===

===start-group=== "counter machine: COPY operation"

  --test-- "counter-machine-test-19"
	reset "CM"
	execute-code [
	        CONST:2,2
		a:  JZ:b,2
			DEC:2
			INC:3
			INC:1
			JUMP:a
		b:  JZ:z,1
			DEC:1
			INC:2
			JUMP:b
		z:  HALT
	]
  --assert "make vector! [0 2 2 0 0]" = mold copy/part mem 5

===end-group===

~~~end-file~~~
