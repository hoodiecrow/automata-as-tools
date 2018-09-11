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

This is a finite-state machine from a Wikipedia example. It recognizes sequences of 1 and 0 symbols that contain an even number of 0s (`(00|1)*`).

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

This is a finite state transducer from another Wikipedia example. It recognizes
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

This is a PDA which recognizes the language {0<sup>n</sup>1<sup>n</sup> n ≥ 0} (not a regular language):

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

