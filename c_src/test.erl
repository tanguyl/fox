C = fox:array(lists:seq(1,3), [3, 1, 1]),
     D = fox:array(lists:seq(1,3), [3, 1]),
     E = fox:array([2,3,4,3,4,5,4,5,6], [3,3,1]),
     fox:to_lists(fox:op("add",C,D)).

-> LOOP:

void op_add(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++)
        dest.content[i*dest.stride_r] = lhs.content[i*lhs.stride_r modulo shape] ... rhs.content[i*rhs.stride_r modulo shape];
}


// A basic library
-> ARRAYS OF SAME DIMENSION/SHAPE
-> STRIDE IS ALWAYS 1

IN ERLANG
1- prepend ones to have "same number of dimensions"
2- determine output shape, number of elements
3- "merge" from right to left: whilst r,h both 1/same dimension: shape-1 *= shape
4- shape-1 *= shape

IN C:
-> Arrays:stride_r = always 1
-> Arrays:shape_r

void op_add(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++)
        dest.content[i] = lhs.content[i modulo lhs.shape_r] + rhs.content[i modulo rhs.shape_r];
}


1000 1    1    3    1
     100  10   3    1

1000 1    1  3
1    100  10 3

1000 1    3
1    100  30

1000 3
1    3000

-> both have more then 1 dim left
     if "same dim"
          shift left both
     if "dif dim"
          if none equal to 1 error
          if one dim equal to 1, dif dimm in "same direction":
               shift left both
          else:
               stop iterating


