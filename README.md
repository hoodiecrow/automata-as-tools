# automata-as-tools
Experiments with making tools based on different classes of automata/abstract machines.

*Actually, the tool-making part is less relevant at the moment, but I think it will resurface.*

Mostly done, I think:

* Finite state machine
* Finite state transducer
* Push-down automaton
* (Basic) Turing machine
* Post-Turing machine

Doing

* Wang B-machine
* Register machine
* Stack machine
* Counter machine

Prospective

* Karel robot
* SECD machine
* Queue automaton
* Counter automaton

## Classes overview

See Wiki for slightly more detailed documentation.

## Implementation

The machines here are (so far) built around a "state transition engine" which handles non-deterministic state transitioning (and, trivially, deterministic s.t. as a degenerate case). Currently, it supports output based on state and input (as a Mealy machine, though a Moore machine can be simulated by coordinating for input). It supports 1½ input streams: by using one stream bi-directionally, e.g. pushdown automata can be supported.

I have undoubtedly made many mistakes here. It is at this point mostly a project of exploration.

## References

For consistency, I've used Wikipedia articles (not that those are always consistent), occasionally correcting against other articles.

https://en.wikipedia.org/wiki/Abstract_machine
https://en.wikipedia.org/wiki/Finite-state_machine
https://en.wikipedia.org/wiki/Pushdown_automaton
https://en.wikipedia.org/wiki/Turing_machine

Everything in this project can be reached from pages linked to by those pages.

## Examples

All examples are from Wikipedia articles unless otherwise stated.

### Finite state machine

This is a finite-state machine which recognizes sequences of 1 and 0 symbols that contain an even number of 0s (`(00|1)*`).

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

The machine can be printed:

```
M print
# =>
A Input alphabet : {0, 1}
Q State symbols  : {s1, s2}
S Start symbol(s): {s1}
F Final symbol(s): {s1}
Transitions
q0  inp q1  out
s1  0   s2  
s1  1   s1  
s2  0   s1  
s2  1   s2  
```

### Finite state transducer

This is a finite state transducer. It recognizes
a regular relation A = {0, 1}, B = {0, 1} such that for state *q* and input
*a*, output is ω(s<sub>i</sub>, a: a ∈ A) = 0, ω(s<sub>0</sub>, 1) = 1,
ω(s<sub>1</sub>, 0) = 1, ω(s<sub>0</sub>, 0) = 0, and ω(s<sub>1</sub>, 1) = 0.
Simply put, it's an edge detector that outputs 1 if the input changes and 0 if
it stays the same.

<a title="By עברית: עצמי. (עברית: תמונה זו נוצרה בעזרת OmniGraffle.) [GFDL (http://www.gnu.org/copyleft/fdl.html) or CC BY-SA 3.0 
 (https://creativecommons.org/licenses/by-sa/3.0
)], via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:Mealy.png"><img width="128" alt="Mealy" src="https://upload.wikimedia.org/wikipedia/commons/b/b4/Mealy.png"></a>

```
::automata::FST create M
foreach {q0 sym q1 out} {
    si 0 s0 0
    si 1 s1 0
    s0 0 s0 0
    s0 1 s1 1
    s1 0 s0 1
    s1 1 s1 0
} {
    M T set $q0 $sym $q1 $out
}
M S set si
M F set s0 s1
```

The sequences `1 1 0` and `0 0 1` are both translated to `0 0 1`:

```
M translate {1 1 0}
# => {0 0 1}
```

also, the same machine can recognize that two sequences fulfill the transition relation and reconstruct the input sequence(s):

```
M recognize {1 1 0} {0 0 1}
# => 1

M reconstruct {0 0 1}
# => {{0 0 1} {1 1 0}}
```

It can also generate different valid input/output given a number of steps to take:

```
M generate 3
```

results in the input/output combinations:

```
{0 0 0} {0 0 0}
{0 0 1} {0 0 1}
{0 1 0} {0 1 1}
{0 1 1} {0 1 0}
{1 0 0} {0 1 0}
{1 0 1} {0 1 1}
{1 1 0} {0 0 1}
{1 1 1} {0 0 0}
```

### Pushdown Automaton

This is a PDA which recognizes the language {0<sup>n</sup>1<sup>n</sup> | n ≥ 0} (not a regular language):

<a title="By Jochgem [CC BY-SA 3.0 
 (https://creativecommons.org/licenses/by-sa/3.0
) or GFDL (http://www.gnu.org/copyleft/fdl.html)], from Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:Pda-example.svg"><img width="256" alt="Pda-example" src="https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Pda-example.svg/256px-Pda-example.svg.png"></a>


```
A Input alphabet : {0, 1}
B Stack alphabet : {A, Z}
Q State symbols  : {p, q, r}
Z Stack bottom   : Z
S Start symbol(s): {p}
F Final symbol(s): {r}
Transitions
q0  inp q1  out
p   0   p   Z A Z
p   0   p   A A A
p   ε   q   Z Z
p   ε   q   A A
q   1   q   A
q   ε   r   Z Z
```

It is defined thus:

```
::automata::PDA create M
M T set p 0 p Z A Z
M T set p 0 p A A A
M T set p ε q Z Z
M T set p ε q A A
M T set q 1 q A
M T set q ε r Z Z
M S set p
M Z set Z
M F set r
```


```
M accept {1 1 0}
# => 0
M accept {0 1}
# => 1
```

### Basic Turing Machine

This is a basic Turing machine which implements a 3-state, 2-symbol "Busy Beaver":

```
A Tape alphabet  : {0, 1}
B Init alphabet  : {0, 1}
b Blank symbol   : 0
Q State symbols  : {A, B, C, H}
S Start symbol   : A
F Final symbol(s): {H}
Transitions
q0  inp q1  out
A   0   B   1 R
A   1   C   1 L
B   0   A   1 L
B   1   B   1 R
C   0   B   1 L
C   1   H   1 R
```

It is defined as


```
::automata::BTM create M
M T set A 0 B 1 R
M T set A 1 C 1 L
M T set B 0 A 1 L
M T set B 1 B 1 R
M T set C 0 B 1 L
M T set C 1 H 1 R
M S set A
M b set 0
M F set H
```

and produces:

```
M run {0}
# => {1 1 1 1 1 1 0} 1 H
```

I.e. the sequence 1 1 1 1 1 1 0, ending with the reader in position #1 and state H.

### Post-Turing Machine

This is the code for a 2-state Busy Beaver:

```
::automata::PTM create M
M compile {
A:  J1:+4
    P
    R
    J:B
    P
    L
    J:B
B:  J1:+4
    P
    L
    J:A
    P
    N
    J:H
H:  H
}
```

It produces a list of final tape contents and head position:

```
M run {0} 0
# => {1 1 1 1} 1
```
