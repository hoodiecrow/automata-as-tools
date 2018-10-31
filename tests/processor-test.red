Red [
  Title:   "Testing processor.red"
  File:    %processor-test.red
]

do %/C/users/peter/code/red/quick-test/quick-test.red
#include append what-dir %../source/processor.red

~~~start-file~~~ "processor"


===start-group=== "check pointers"


  --test-- "processor-test-1"

  --assert 0 = pointers/ap

  --test-- "processor-test-2"

	incptr 'ap
  --assert 1 = pointers/ap


  --test-- "processor-test-3"

	decptr 'ap
	decptr 'ap
	decptr 'ap
  --assert 0 = pointers/ap

  --test-- "processor-test-4"

	incptr 'ap
	memset 'ap 3
  --assert 3 = memory/1

  --test-- "processor-test-5"
	incptr 'ap
	memset 'ap 2
  --assert 2 = memget 'ap

  --test-- "processor-test-6"
    resetptr 'sp
	incptr 'sp memset 'sp 3
	incptr 'sp memset 'sp 2
  --assert "make vector! [3 2 0 0 0 0 0 0]" = mold copy/part memory 8

  --test-- "processor-test-7"
    resetptr 'sp
	incptr 'sp memset 'sp 3
	incptr 'sp memset 'sp 2
	copyptr 'bp 'sp
	decptr 'sp
	copyptr 'cp 'sp
  --assert [2 3] = append append [] memget 'bp memget 'cp

  --test-- "processor-test-8"
    resetptr 'sp
	incptr 'sp memset 'sp 3
	incptr 'sp memset 'sp 2
	copyptr 'bp 'sp
	decptr 'sp
	copyptr 'cp 'sp
	copyptr 'ap 'sp
	memset 'ap ADD memget 'bp memget 'cp
  --assert "make vector! [5 2 0 0 0 0 0 0]" = mold copy/part memory 8

  --test-- "processor-test-9"
    resetptr 'sp
	incptr 'sp memset 'sp 4
	incptr 'sp memset 'sp 3
	incptr 'sp memset 'sp 2
	copyptr 'bp 'sp
	decptr 'sp
	copyptr 'cp 'sp
	copyptr 'ap 'sp
	memset 'ap ADD memget 'bp memget 'cp
	copyptr 'bp 'sp
	decptr 'sp
	copyptr 'cp 'sp
	copyptr 'ap 'sp
	memset 'ap MULTIPLY memget 'bp memget 'cp
  --assert "make vector! [20 5 2 0 0 0 0 0]" = mold copy/part memory 8

===end-group===

~~~end-file~~~
