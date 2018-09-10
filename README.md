# automata-as-tools
Experiments with making tools based on different classes of automata/abstract machines.

*Actually, the tool-making part is less relevant at the moment, but I think it will resurface.*

Mostly done, I think:

* Finite state machine
* Finite state transducer
* Push-down automaton

Doing

* Post-Turing machine
* (Basic) Turing machine

Prospective

* Karel robot
* SECD machine
* Register machine
* Stack machine
* Counter machine
* Queue automaton
* Counter automaton

## Classes overview

See Wiki for slightly more detailed documentation.

## Examples

### Finite state machine

This is a finite-state machine from a Wikipedia example. It accepts sequences of 1 and 0 symbols that contain an even number of 0s.

<a title="By Cepheus [Public domain], from Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:DFAexample.svg"><img width="256" alt="DFAexample" src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/DFAexample.svg/256px-DFAexample.svg.png"></a>

```
::automata::FSM create M
foreach {q0 sym q1} {
    s1 0 s2
    s1 1 s1
    s2 0 s1
    s2 1 s2
} {
    M T set $q0 $sym $q1
}
M S set s1
M F set s1
```

The queries

```
M accept {0 1}
M accept {0 0}
```

return 0 and 1, respectively.
