Red [
  Title:   "Testing processor.red"
  File:    %processor-test.red
]

do %/C/users/peter/code/red/quick-test/quick-test.red
#include append what-dir %../source/processor.red

~~~start-file~~~ "processor"


===start-group=== "check pointers"


  --test-- "processor-test-1" --assert 0 = p/a

  --test-- "processor-test-2" 
	++ p/a
  --assert 1 = p/a


  --test-- "processor-test-3" 
	-- p/a
	-- p/a
	-- p/a
  --assert 0 = p/a

  --test-- "processor-test-4" 
	++ p/a
	mem/(:p/a): 3
  --assert 3 = mem/1

  --test-- "processor-test-5"
	++ p/a
	mem/(:p/a): 2
  --assert 2 = mem/(:p/a)

  --test-- "processor-test-6"
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part mem 8

  --test-- "processor-test-7"
    p/s: 0
	++ p/s mem/(:p/s): 3
	++ p/s mem/(:p/s): 2
	p/b: p/s
	-- p/s
	p/c: p/s
  --assert [2 3] = append append [] mem/(:p/b) mem/(:p/c)

  --test-- "processor-test-8"
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
    execute 0 0 1 2
  --assert 1 = p/i

  --test-- "processor-test-10"
    execute 0 0 1 2
    execute 0 0 1 2
  --assert 3 = p/i

===end-group===

~~~end-file~~~
