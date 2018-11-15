Red [
  Title:   "Testing processor.red"
  File:    %processor-test.red
]

do %/C/users/peter/code/red/quick-test/quick-test.red
#include append what-dir %../source/processor.red

~~~start-file~~~ "processor"


===start-group=== "stack machine"


  --test-- "stack-machine-test-13"
	M: make SMProc []
		M/mset 1 3
		M/mset 2 2
		M/sp: 2
	M/execute [
		ADD
		HALT
	]
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part M/mem 8

  --test-- "stack-machine-test-14"
    execute [
		reset "SM"
		poke mem 1 4
		poke mem 2 3
		poke mem 3 2
		set 'sp 3
	][
		ADD
		MUL
		HALT
	]
  --assert "make vector! [20 5 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-15"
    execute [
		reset "SM"
	][
		PUSH:2 PUSH:3
		ADD
		PUSH:4
		MUL
		HALT
	]
  --assert "make vector! [20 4 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-16"
    execute [
		reset "SM"
		poke mem 1 3
		set 'sp 1
	][
		CLEAR
		CONST:2
		DUP
		HALT
	]
  --assert "make vector! [0 2 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-17"
    execute [
		reset "SM"
		poke mem 1 3
		poke mem 2 2
		set 'sp 2
	][
		CMP
		HALT
	]
  --assert -1 = cmp

  --test-- "stack-machine-test-18"
    execute [
		reset "SM"
		poke mem 1 2
		poke mem 2 3
		set 'sp 2
	][
		CMP
		HALT
	]
  --assert 1 = cmp

  --test-- "stack-machine-test-19"
	comment {Implement a CLR operation, yes, could be DEC;JNZ:-1}
    execute [
		reset "SM"
	][
				PUSH:3
		a:		JZ:end
				DEC
				JUMP:a
		end:	HALT
	]
  --assert "make vector! [0 0 0 0 0 0 0 0]" = mold copy/part mem 8

===end-group===


===start-group=== "counter machine"

  --test-- "counter-machine-test-14"
    execute [
		reset "CM"
		poke mem 1 3
		poke mem 2 2
	][
		HALT
	]
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-15"
    execute [
		reset "CM"
		poke mem 1 3
		poke mem 2 2
	][
		ADD:3,2,1
		HALT
	]
  --assert "make vector! [3 2 5 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-16"
    execute [
		reset "CM"
		poke mem 1 3
		poke mem 2 2
	][
	    CLEAR:1
	    COPY:3,2
		HALT
	]
  --assert "make vector! [0 2 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-17"
    execute [
		reset "CM"
		poke mem 1 3
		poke mem 2 2
	][
		CMP:3,2,1
		HALT
	]
  --assert -1 = cmp

  --test-- "counter-machine-test-18"
    execute [
		reset "CM"
		poke mem 1 2
		poke mem 2 3
	][
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

  --test-- "counter-machine-test-19"
    comment {Implement a CPY operation}
	execute [
		reset "CM"
		poke mem 2 2
	][
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

  --test-- "counter-machine-test-20"
    comment {Implement a DEC operation using instruction set 2}
	execute [
		reset "CM"
		poke mem 2 2
	][
		    INC:1
		a:  JEQ:b,2,3
			INC:3
			JEQ:a,0,0
		b:  CLEAR:2
		c:  JEQ:z,1,3
			INC:1
			INC:2
			JEQ:c,0,0
		z:  HALT
	]
  --assert "make vector! [2 1 2 0 0]" = mold copy/part mem 5

===end-group===

===start-group=== "post-turing machine"

  --test-- "post-turing-machine-test-1"
    comment {2 state, 2 symbol busy beaver}
	execute [
		reset "PTM"
	][
		A:	J1:+3,0
			OUT:head,P,R
			JUMP:B
			OUT:head,P,L
			JUMP:B
		B:	J1:+3,0
			OUT:head,P,L
			JUMP:A
			OUT:head,P
			NOP
			JUMP:H
		H:	HALT
	]
  --assert "make vector! [1 1 1 1 0]" = mold copy/part tape 5

===end-group===

~~~end-file~~~
