Red [
  Title:   "Tests of foo.red"
  File:    %foo-test.red
]

inc: func [n][n: n + 1]
; C:\Users\Peter\code\red\quick-test
do %/C/users/peter/code/red/quick-test/quick-test.red
;#include %~/code/red/quick-test/quick-test.red
;#include %relativepathto/inc.reds

~~~start-file~~~ "inc"                          ;; start test file  
                                                ;;  initialises totals

===start-group=== "increment an variable"       ;; a marker to group tests
                                                ;;  group name [string!]

  --test-- "inc-test-1"                         ;; start of code for a test
                                                ;;   test name [string!]
    i: 1
  --assert 2 = inc i                            ;; an assertion
                                                ;;   expression [logic!]

===end-group===                                 ;; end of group marker                                                  

~~~end-file~~~                                  ;; finish test - print totals
