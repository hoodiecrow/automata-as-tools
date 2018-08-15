
oo::class create Set {
    variable data

    constructor args {
        my set $args
    }

    method set values {
        set data {}
        my add $values
    }

    method add values {
        foreach value $values {
            if {$value ne {}} {
                dict set data $value 1
            }
        }
    }

    method empty {} {
        expr {[my size] == 0}
    }

    method size {} {
        dict size $data
    }

    method all {} {
        dict keys $data
    }

    method exists key {
        dict exists $data $key
    }

    method intersects stateset {
        my for s {
            if {$s in $stateset} {
                return 1
            }
        }
        return 0
    }

    # TODO might need to cover more return codes in for and map
    method for {varName script} {
        upvar 1 $varName var
        foreach var [dict keys $data] {
            try {
                uplevel 1 $script
            } on return res {
                return -level 2 $res
            }
        }
    }

    method map {varName script} {
        upvar 1 $varName var
        set res [Set new]
        foreach var [dict keys $data] {
            $res add [uplevel 1 $script]
        }
        return $res
    }

}
