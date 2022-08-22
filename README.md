# fox
A ndarray library for Erlang, written using dirty nefs, and still in early stages.
Currently implemented functions:
``` array/1, array/2, btl/2, ltb/2, cast_array/2, linspace/3, op/2, op/3```

# Arrays
Arrays are represented as ```-record(array,{content, shape, stride}).```. 

## Building arrays
The ```erlang fun array/1``` accept for arguments
* a nested list of number, creating an array of matching dimensions
* a number, creating a ndarray of a single value
* a binary, creating a single dimension ndarray.

Alternatively,  ```erlang fun array/2 (Content, Shape)``` takes for arguments
* Content, a number | list of number | binary of ³floats
* Shape, a list of ints indicating the ndarray shape.

Finally, the function ```fun linspace/3(Start, Stop, Nsamples)``` can be used to create a binary usable as a an array's content field faster then ```fun lists:seq```.
```erlang
Lin     = fox:array(fox:linspace(0,99,100)).  % [0,1,...,99].
```

## Array formats
Nifs operate on binaries, but arrays can be switched to list representation at will with ```fun cast_array/2```.

```erlang
A = array([[1,2]]).
cast_array(A, c).       % {array,<<0,0,0,0,0,0,240,63,0,0,0,0,0,0,0,64>>,<<1,0,0,0,2,0,0,0>>,<<2,0,0,0,1,0,0,0>>}
cast_array(A, erlang).  % {array,<<0,0,0,0,0,0,240,63,0,0,0,0,0,0,0,64>>,[1,2],[2,1]}
cast_array(A, list).    % {array,[1.0,2.0],[1,2],[2,1]}
```

The ```fun ltb/2, fun btl/2``` (ListToBinary/BinaryToList) can be used to switch number sequence from/to numbers:
```erlang
fox:ltb([1], d).                     % d-> write as double:   <<0,0,0,0,0,0,240,63>>
fox:ltb([1], i).                     % i-> write as int:      <<1,0,0,0>>
fox:btl(<<0,0,0,0,0,0,240,63>>,d).   % d-> read as double:    [1]
fox:btl( <<1,0,0,0>>, i).            % i-> read as int:       [1.0]
```

## Examples
```erlang
One     = fox:array(1),                       % [1.0].
Two     = fox:array([2]),                     % [2.0].
Vector  = fox:array([1,2,3]),                 % [1.0, 2.0, 3.0]
Matrix  = fox:array(lists:seq(1,6), [2,3]).   % [[1,2,3], [4,5,6]].
Matrix2 = fox:array([[1,2], [3,4]]).          % [[1,2],[3,4]]. 
Lin     = fox:array(fox:linspace(0,99,100)).  % [0,1,...,99].
```

# Unary ops
The following operations perform a 1-to-1 transformation of input arrays: ```acos,acosh,asin,asinh,atan,atan2,atanh,ceil,cos,cosh,exp,floor,fmod,log,log10,log2,sin,sinh,sqrt,tan,tanh```

## Examples

They can be used trough the ```op/2``` function:
```erlang
R = fox:op("cos",Matrix).
S = fox:op("sin", 1).
T = fox:op("tan", fox:linspace(0,3.141,4)).
```

# Binary ops
## Broadcasting
Operations can be broadacasted on array if their shapes are compatible: from right to left, their dimensions should be the same/either equal to one, the absence of a dimension is treated as a 1. See a detailed example from  [numpy](https://numpy.org/doc/stable/user/basics.broadcasting.html).

```erlang
N = 100,
B = fox:linspace(0,N-1,N),

L = fox:array(B, [N,1,1]),
R = fox:array(B, [N,1]),

F = numerl:apply_op("add", L,R).
```
For N = 100, F is an array of shape [100, 100, 1].


## Implemented ops
The following functions have been implemented: ```add, sub, mult, div, pow```.

```erlang
Addition        = fox:op("add", Matrix, 1),
Substraction    = fox:op("sub", Matrix, [1,2,3]),
Division        = fox:op("div", Matrix, Vector),
Multiplication  = fox:op("mult",Matrix, Matrix),
Power           = fox:op("pow", Matrix, -1).
```

# "Reduction"
In work; currently supported: sum an array alongside it's rightmost axis.

```erlang
fox:reduce([[1,2,3],[4,5,6]]).  %[6,15]
```

# Todo
Broadcasting: Optimize "inner loop".
Slice:        implement.
Reduce:       add more operation (sum, squared length, mean, norm...), add all axis.
