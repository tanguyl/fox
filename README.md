# fox
A ndarray library for Erlang.

# Arrays
Arrays are represented using binaries. They can be created as such:

```erlang
One = fox:array(1),                       % One: [1.0].
Two = fox:array([2]),                     % Two: [2.0
Big = fox:array(lists:seq(1,10), [5,2]).  % Big: [[1,2,3,4,5], [6,7,8,9,10]]
```

# Operations
The addition, substraction, multiplication, division operators are supported: ('+', '-', '/', '*'):

```erlang
Two   = fox:apply_op('+', 1, 1),
Three = fox:apply_op('-', Big, Two),
Four  = fox:apply_op('/', Big, 2),
Five  = fox:apply_op('*', Big, Big).
```

# Broadcasting
Operations can be broadacasted on array if their shapes are compatible: from right to left, their dimensions should be the same/one of the dimension should be one, the absence of a dimension being treated as a 1. For example:


```erlang
N = 100,
B = lists:seq(1,N),
L = fox:array(B, [N,1,1]),
R = fox:array(B, [N,1]),
F = numerl:apply_op('+', L,R).
```
For N = 100, F is an array of shape [100, 100, 1].

