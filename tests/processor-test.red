Red [
  Title:   "Testing processor.red"
  File:    %processor-test.red
]

do %/C/users/peter/code/red/quick-test/quick-test.red
#include append what-dir %../source/processor.red

~~~start-file~~~ "processor"


===start-group=== "check pointers"


	reset
  --test-- "processor-test-1" --assert 0 = p/a

  --test-- "processor-test-2" 
	reset
	++ p/a
  --assert 1 = p/a


  --test-- "processor-test-3" 
	reset
	-- p/a
	-- p/a
	-- p/a
  --assert 0 = p/a

  --test-- "processor-test-4" 
	reset
	++ p/a
	mem/(:p/a): 3
  --assert 3 = mem/1

  --test-- "processor-test-5"
	reset
	++ p/a
	mem/(:p/a): 2
  --assert 2 = mem/(:p/a)

  --test-- "processor-test-6"
	reset
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "processor-test-7"
	reset
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
	p/b: p/s
	-- p/s
	p/c: p/s
  --assert [2 3] = append append [] mem/(:p/b) mem/(:p/c)

  --test-- "processor-test-8"
	reset
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
	p/b: p/s
	-- p/s
	p/c: p/s
	p/a: p/s
	mem/(:p/a): ADD mem/(:p/b) mem/(:p/c)
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "processor-test-9"
	reset
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

  --test-- "processor-test-10"
	reset
    execute 'NOP 0 1 2
  --assert 1 = p/i

  --test-- "processor-test-11"
	reset
    execute 'NOP 0 1 2
    execute 'NOP 0 1 2
  --assert 2 = p/i

  --test-- "processor-test-12"
	reset
    p/s: 9
    execute 'NOT 0 1 2
  --assert 9 = p/a

  --test-- "processor-test-13"
	reset
    execute 'CONST 3 0 0
  --assert "make vector! [3 0 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "processor-test-14"
	reset
    execute 'CONST 3 0 0
    execute 'CONST 2 0 0
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "processor-test-15"
	reset
    execute 'CONST 3 0 0
    execute 'CONST 2 0 0
    execute 'ADD 0 0 0
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "processor-test-16"
	reset
    execute 'CONST 3 0 0
    execute 'CLEAR 0 0 0
    execute 'CONST 2 0 0
    execute 'DUP 0 0 0
  --assert "make vector! [0 2 2 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "processor-test-17"
	reset
    execute 'CONST 3 0 0
    execute 'CONST 2 0 0
    execute 'CMP 0 0 0
  --assert -1 = cmp

  --test-- "processor-test-18"
	reset
    execute 'CONST 2 0 0
    execute 'CONST 3 0 0
    execute 'CMP 0 0 0
	print p
	print mem
  --assert 1 = cmp

===end-group===

~~~end-file~~~
