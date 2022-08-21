# fox
A ndarray library for Erlang.

# Arrays
Arrays are represented using binaries. They can be created as such:

```erlang
One     = fox:array(1),                       % [1.0].
Two     = fox:array([2]),                     % [2.0].
Vector  = fox:array([1,2,3]),                 % [1.0, 2.0, 3.0]
Matrix  = fox:array(lists:seq(1,6), [2,3]).   % [[1,2,3], [4,5,6]].
Matrix2 = fox:array([[1,2], [3,4]]).          % [[1,2],[3,4]]. 
```
The ```erlang fun array/1``` accept for arguments
* a nested list of number
* a list of number, creating a ndarray of single dimension
* a number, creating a ndarray of a single value

Alternatively,  ```erlang fun array/2 (Content, Shape)``` takes for arguments
* Content, a list of number
* Shape, a list of ints indicating the ndarray shape.

# Basic operations
The following operations perform a 1-to-1 transformation of input arrays: ```acos,acosh,asin,asinh,atan,atan2,atanh,ceil,cos,cosh,exp,floor,fmod,log,log10,log2,sin,sinh,sqrt,tan,tanh```

They can be used trough the ```op/2``` function:
```erlang
R = fox:op("cos",Matrix).
```

# Broadcasting
Operations can be broadacasted on array if their shapes are compatible: from right to left, their dimensions should be the same/either equal to one, the absence of a dimension being treated as a 1. For example:


```erlang
N = 100,
B = lists:seq(1,N),

L = fox:array(B, [N,1,1]),
R = fox:array(B, [N,1]),

F = numerl:apply_op('+', L,R).
```
For N = 100, F is an array of shape [100, 100, 1].

# Operations
The following operations can be broadcasted:

```erlang
Addition        = fox:apply_op('+',   Matrix, 1),
Substraction    = fox:apply_op('-',   Matrix, Two),
Division        = fox:apply_op('/',   Matrix, Vector),
Multiplication  = fox:apply_op('*',   Matrix, Matrix),
Power           = fox:apply_op('pow', Matrix, -1).
```

# Todo
Broadcasting: simplify operations on shapes of type [X,1] [Y,Z]: currently looping on Z.
Reduce:       correct
Slice:        implement
