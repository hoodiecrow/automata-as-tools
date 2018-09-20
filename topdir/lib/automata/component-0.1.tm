oo::class create ::automata::Component {
    variable data label scalar superset domain exclude

    #: This class is used for most of the values that make up the
    #: machine-defining tuples. The value itself can be a scalar or a set (set
    #: is default).
    #:
    constructor args {
        set scalar 0
        set superset {}
        set domain {}
        set exclude {}
        while {[llength $args] > 0} {
            #: The following options are recognized by the constructor:
            switch [lindex $args 0] {
                -label {
                    #: * `-label str` : stores a description string.
                    set args [lassign $args - label]
                }
                -scalar {
                    #: * `-scalar` : sets the value type to *scalar*.
                    set args [lassign $args -]
                    set scalar 1
                }
                -in {
                    #: * `-in cmd` : registers a command prefix. When the
                    #: component's value is set, this command prefix will be
                    #: called as `cmd set val` for every value passed as
                    #: argument to the component's `set` method.
                    set args [lassign $args - superset]
                }
                -domain {
                    #: * `-domain d` : adds a domain test. Currently supports
                    #: the domains N (non-negative integers), Z (integers), R
                    #: (double precision floating point numbers).
                    set args [lassign $args - domain]
                }
                -exclude {
                    #: * `-exclude syms` : the set or scalar will not accept
                    #: the listed symbols (if given an empty list, reject empty
                    #: symbols)
                    set args [lassign $args - exclude]
                }
                default {
                    break
                }
            }
        }

        #:
        #: If any further arguments are given, the first of those will be set
        #: as a scalar component's value, or all of them as a set component's
        #: value.
        if {$scalar} {
            lassign $args data
        } else {
            set data $args
        }
        set ns [namespace qualifiers [self]]
        set name [namespace tail [self]]
        lappend $ns\::complist $name
        oo::objdefine [uplevel 1 {self}] forward $name $name
        oo::objdefine [uplevel 1 {self}] export $name
    }

    method print {} {
        #: Print the component's label, name, and value.
        set _data [lmap v $data {if {$v eq {}} {lindex ε} {set v}}]
        if {[string match *(s) $label] && $scalar} {
            return [format {%-15s %s = %s}   [string range $label 0 end-3] [namespace tail [self]] $_data]
        } elseif {$scalar} {
            return [format {%-15s %s = %s}   $label [namespace tail [self]] $_data]
        } else {                                                            
            return [format {%-15s %s = {%s}} $label [namespace tail [self]] [join [lsort -dict $_data] ", "]]
        }
    }

    method clear {} {
        #: Set the underlying value to {}
        set data {}
    }

    method get {} {
        #: yields the underlying value
        return $data
    }

    method set args {
        #: sets the underlying value by replacing a scalar or inserting new
        #: values in a set. 
        foreach value $args {
            if {$value in $exclude} {
                continue
            }
            switch $domain {
                N { if {![string is digit -strict $value]} { continue } }
                Z { if {![string is entier -strict $value]} { continue } }
                R { if {![string is double -strict $value]} { continue } }
            }
            if {$superset ne {}} {
                $superset set $value
            }
            if {$scalar} {
                set data $value
                return
            } else {
                lappend data $value
            }
        }
        set data [lsort -unique $data]
    }

    method forall {varName cond} {
        #: tests condition *cond* once for every member of the component's
        #: value. The value of *var* is set to the value of the item when
        #: *cond* is evaluated. Returns 1 if every item passes the test, and
        #: aborts testing with a return value of 0 if any test fails.
        upvar 1 $varName var
        foreach var $data {
            if {![uplevel 1 [list expr $cond]]} {
                return 0
            }
        }
        return 1
    }

    method any {varName cond} {
        #: tests condition *cond* once for every member of the component's
        #: value. The value of *var* is set to the value of the item when
        #: *cond* is evaluated. Returns 0 if no item passes the test, and
        #: aborts testing with a return value of 1 if any test passes.
        upvar 1 $varName var
        foreach var $data {
            if {[uplevel 1 [list expr $cond]]} {
                return 1
            }
        }
        return 0
    }

    method contains value {
        #: Tests for *value* being a member of the component's value.
        expr {$value in $data}
    }

    #: 
    #: ## TODO
    #:
    #: this class is largely untested except as part of using other
    #: classes.
    #: 
}
