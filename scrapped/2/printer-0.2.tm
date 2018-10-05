oo::class create ::automata::Printer {
    variable complist

    method print {} {
        #: Print the machine description by printing its components.
        puts [join [lmap c $complist {my $c print}] \n]
    }

}
