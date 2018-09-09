#: ### Component
#: 
#: This class is used for most of the values that make up the machine-defining
#: tuples. The value itself can be a scalar or a set (set is default).
oo::class create ::automata::Component {
    variable data scalar superset nonempty

#: 
#: **Creation:**
#: 
#: The following options are recognized by the constructor:
#: 
#: * `-scalar` : sets the value type to *scalar*.
#: * `-nonempty` : the set or scalar will not accept empty values
#: * `-in cmd` : registers a command prefix. When the component's value is set,
#: this command prefix will be called as `cmd set val` for every value passed
#: as argument to the component's `set` method.
#: 
#: If any further arguments are given, the first of those will be set as a
#: scalar component's value, or all of them as a set component's value.
    constructor args {
        set scalar 0
        set superset {}
        set nonempty 0
        while {[llength $args] > 0} {
            switch [lindex $args 0] {
                -scalar {
                    set args [lassign $args -]
                    set scalar 1
                }
                -in {
                    set args [lassign $args - superset]
                }
                -nonempty {
                    set args [lassign $args -]
                    set nonempty 1
                }
                default {
                    break
                }
            }
        }
        if {$scalar} {
            lassign $args data
        } else {
            set data $args
        }
    }

#: 
#: **Behavior:**
#: 
#: * `get` : yields the underlying value
    method get {} {
        return $data
    }

#: * `set ?value...?` : sets the underlying value by replacing a scalar or
#: inserting new values in a set. 
    method set args {
        foreach value $args {
            if {$value eq {} && $nonempty} {
                continue
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

#: * `forall var cond` : tests condition *cond* once for every member of the
#: component's value. The value of *var* is set to the value of the item when
#: *cond* is evaluated. Returns 1 if every item passes the test, and aborts
#: testing with a return value of 0 if any test fails.
    method forall {varName cond} {
        upvar 1 $varName var
        foreach var $data {
            if {![uplevel 1 [list expr $cond]]} {
                return 0
            }
        }
        return 1
    }

#: * `any var cond` : tests condition *cond* once for every member of the
#: component's value. The value of *var* is set to the value of the item when
#: *cond* is evaluated. Returns 0 if no item passes the test, and aborts
#: testing with a return value of 1 if any test passes.
    method any {varName cond} {
        upvar 1 $varName var
        foreach var $data {
            if {[uplevel 1 [list expr $cond]]} {
                return 1
            }
        }
        return 0
    }

#: 
#: **TODO:** this class is largely untested except as part of using other
#: classes.
#: 
}
