package require tcltest

set testdir [file dirname [file normalize [info script]]]
set project [lindex [split [file tail [file dirname $testdir]] -] 0]
set libdir  [file join $testdir .. topdir lib]

set outfile [file join $testdir testreport.txt]
set errfile [file join $testdir testerrors.txt]
file delete -force $outfile $errfile

lappend argv -testdir $testdir
lappend argv -outfile $outfile
lappend argv -errfile $errfile
lappend argv -tmpdir [file join $testdir temp]
lappend argv -load [subst -nocommands {
    ::tcl::tm::path add $libdir
    set auto_path [linsert \$auto_path 0 $libdir]
    package require $project
    package require log
}]
# requires mod to tcltest
lappend ::argv -encoding utf-8

::tcltest::configure {*}$argv
::tcltest::runAllTests
