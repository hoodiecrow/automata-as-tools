Red [
  Title:   "Testing processor.red"
  File:    %processor-test.red
]

do %/C/users/peter/code/red/quick-test/quick-test.red
#include append what-dir %../source/processor.red

~~~start-file~~~ "processor"


===start-group=== "stack machine"


	reset "SM"
  --test-- "stack-machine-test-1" --assert 0 = p/a

  --test-- "stack-machine-test-2" 
	reset "SM"
	++ p/a
  --assert 1 = p/a


  --test-- "stack-machine-test-3" 
	reset "SM"
	-- p/a
	-- p/a
	-- p/a
  --assert 0 = p/a

  --test-- "stack-machine-test-4" 
	reset "SM"
	++ p/a
	mem/(:p/a): 3
  --assert 3 = mem/1

  --test-- "stack-machine-test-5"
	reset "SM"
	++ p/a
	mem/(:p/a): 2
  --assert 2 = mem/(:p/a)

  --test-- "stack-machine-test-6"
	reset "SM"
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-7"
	reset "SM"
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
	p/b: p/s
	-- p/s
	p/c: p/s
  --assert [2 3] = append append [] mem/(:p/b) mem/(:p/c)

  --test-- "stack-machine-test-8"
	reset "SM"
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
	p/b: p/s
	-- p/s
	p/c: p/s
	p/a: p/s
	mem/(:p/a): ADD mem/(:p/b) mem/(:p/c)
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-9"
	reset "SM"
    p/s: 0
	++ p/s mem/(:p/s): 4
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
	p/b: p/s
	-- p/s
	p/c: p/s
	p/a: p/s
	mem/(:p/a): ADD mem/(:p/b) mem/(:p/c)
	p/b: p/s
	-- p/s
	p/c: p/s
	p/a: p/s
	mem/(:p/a): MULTIPLY mem/(:p/b) mem/(:p/c)
  --assert "make vector! [20 5 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-10"
	reset "SM"
    execute 'NOP []
  --assert 1 = p/i

  --test-- "stack-machine-test-11"
	reset "SM"
    execute 'NOP []
    execute 'NOP []
  --assert 2 = p/i

  --test-- "stack-machine-test-12"
	reset "SM"
    p/s: 9
    execute 'NOT [0]
  --assert 9 = p/a

  --test-- "stack-machine-test-13"
	reset "SM"
    execute 'CONST [3]
  --assert "make vector! [3 0 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-14"
	reset "SM"
    execute 'CONST [3]
    execute 'CONST [2]
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-15"
	reset "SM"
    execute 'CONST [3]
    execute 'CONST [2]
    execute 'ADD []
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-16"
	reset "SM"
    execute 'CONST [3]
    execute 'CLEAR []
    execute 'CONST [2]
    execute 'DUP []
  --assert "make vector! [0 2 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "stack-machine-test-17"
	reset "SM"
    execute 'CONST [3]
    execute 'CONST [2]
    execute 'CMP []
  --assert -1 = cmp

  --test-- "stack-machine-test-18"
	reset "SM"
    execute 'CONST [2]
    execute 'CONST [3]
    execute 'CMP []
  --assert 1 = cmp

===end-group===

===start-group=== "counter machine"


  --test-- "counter-machine-test-6"
	reset "CM"
	p/c: 1
	p/b: 2
	mem/(:p/c): 3
    mem/(:p/b): 2
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-8"
	reset "CM"
	p/c: 1
	p/b: 2
	p/a: 3
	mem/(:p/c): 3
    mem/(:p/b): 2
	mem/(:p/a): ADD mem/(:p/b) mem/(:p/c)
  --assert "make vector! [3 2 5 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-9"
    reset "CM"
	insert mem [4 3 2]
	mem/4: ADD mem/3 mem/2
	mem/4: MULTIPLY mem/4 mem/1
  --assert "make vector! [4 3 2 20 0 0 0 0]" = mold copy/part mem 8 

  --test-- "counter-machine-test-10"
	reset "CM"
	execute 'NOP []
  --assert 1 = p/i

  --test-- "counter-machine-test-11"
	reset "CM"
	execute 'NOP []
	execute 'NOP []
  --assert 2 = p/i

  --test-- "counter-machine-test-12"
	reset "CM"
	mem/1: 1
	execute 'NOT [1]
  --assert 0 = mem/(:p/a)

  --test-- "counter-machine-test-13"
	reset "CM"
	execute 'CONST [3 1]
  --assert "make vector! [3 0 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-14"
	reset "CM"
	execute 'CONST [3 1]
	execute 'CONST [2 2]
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-15"
	reset "CM"
	execute 'CONST [3 1]
	execute 'CONST [2 2]
	execute 'ADD [3 2 1]
  --assert "make vector! [3 2 5 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-16"
	reset "CM"
	execute 'CONST [3 1]
	execute 'CLEAR [1]
	execute 'CONST [2 2]
	execute 'COPY [3 2]
  --assert "make vector! [0 2 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "counter-machine-test-17"
	reset "CM"
	execute 'CONST [3 1]
	execute 'CONST [2 2]
	execute 'CMP [3 2 1]
  --assert -1 = cmp

  --test-- "counter-machine-test-18"
	reset "CM"
	execute 'CONST [2 1]
	execute 'CONST [3 2]
	execute 'CMP [3 2 1]
	print p
	print mem
  --assert 1 = cmp

comment {

}

===end-group===

~~~end-file~~~
