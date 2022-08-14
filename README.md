# fox
A ndarray library for Erlang.

# Arrays
Arrays are represented using binaries. They can be created as such:

```erlang
One     = fox:array(1),                       % One:   [1.0].
Two     = fox:array([2]),                     % Two:   [2.0].
Vector  = fox:array([1,2,3]),                 % Vector: [1.0, 2.0, 3.0]
Matrix  = fox:array(lists:seq(1,6), [2,3]).   % Matrix:   [[1,2,3], [4,5,6]].
```
The ``` fun array/1``` accept for arguments
* a list of number, creating a ndarray of single dimension
* a number, creating a ndarray of a single value

Alternatively,  ``` fun array/2 (Content, Shape)``` takes for arguments
* Content, a list of number
* Shape, a list of ints indicating the ndarray shape.

# Broadcasting
Operations can be broadacasted on arrays if their shapes are compatible: from right to left, their dimensions should be the same/equal to one, the absence of a dimension being treated as a 1. For example:


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

# Combining operations
 The eval function can be used to combine multiple operations, evaluated left to right. Each operation is expressed 
 under the format ``` [Lhs, Op, Rhs] ```.
 *``` Lhs``` and ```  Rhs``` are compatible ndarrays or operations producing compatible ndarrays
 *``` Op``` is one of the atoms showcased [here](https://github.com/tanguyl/fox/edit/main/README.md#operations).
```erlang
fox:eval([1,'+', 1, '*', 2, '/', 4]).              % ((1+1)*2)/4
fox:eval([ [3,'*', 4], '+', [2, '*', Vector]]).    %  (3*4) + (2*[1,2,3])
```
