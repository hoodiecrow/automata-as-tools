oo::class create ::automata::Component {
    variable data scalar superset nonempty

    #: This class is used for most of the values that make up the
    #: machine-defining tuples. The value itself can be a scalar or a set (set
    #: is default).
    constructor args {
        set scalar 0
        set superset {}
        set nonempty 0
        while {[llength $args] > 0} {
            #: The following options are recognized by the constructor:
            #: 
            switch [lindex $args 0] {
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
                -nonempty {
                    #: * `-nonempty` : the set or scalar will not accept empty
                    #: values
                    set args [lassign $args -]
                    set nonempty 1
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
    }

    method get {} {
        #: yields the underlying value
        return $data
    }

    method set args {
        #: sets the underlying value by replacing a scalar or inserting new
        #: values in a set. 
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

    #: 
    #: ## TODO
    #:
    #: this class is largely untested except as part of using other
    #: classes.
    #: 
}
