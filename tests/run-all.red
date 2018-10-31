Red []

;; supress script messages
store-quiet-mode: system/options/quiet
system/options/quiet: true

do %/C/users/peter/code/red/quick-test/quick-test.red

;; run the tests
print ["REBOL " system/version]
start-time: now/precise
print ["This test started at" start-time]

do append what-dir "foo-test.red"
do append what-dir "processor-test.red"

end-time: now/precise
print ["       in" difference end-time start-time newline]
print ["The test finished at" end-time]
system/options/quiet: store-quiet-mode
