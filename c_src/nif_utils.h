#include <erl_nif.h>
#include <stdio.h>
#include <string.h>
#include "nif_math.h"

//Used for debug purpose.
//Likely thread unsafe.
//Usage: debug_write("A double: %lf, an int:%d", double_val, int_val);
//Fprints to debug.txt
void debug_write(const char* fmt, ...){
    FILE* fp = fopen("debug.txt", "a");
    va_list args;

    va_start(args, fmt);
    vfprintf(fp, fmt, args);
    va_end(args);

    fclose(fp);
}

//Create an exception term.
ERL_NIF_TERM exception(ErlNifEnv* env, const char* format, ...){
    char error[254];

    va_list arg;
    va_start(arg, format);
    vsnprintf(error, 250, format, arg);
    va_end(arg);

    return enif_raise_exception(env, enif_make_atom(env, error));
}

//Allocates a binary for an array of input shape.
// Required space: each element stored as double, 1 int for array size, 1 int for number of dims;
//                   n_dims ints for each dimension size, n_dims ints for each stride.
ERL_NIF_TERM alloc_array_binary(ErlNifEnv* env, ErlNifBinary* dest, int size, int n_dims){
    size_t required_space = sizeof(double)*size + sizeof(int)*(2+2*n_dims);

    ErlNifBinary temp;
    if(!enif_alloc_binary(required_space, &temp))        
        return exception(env, "Could not create binary.\n");

    *dest = temp;
    int *ptr = (int*) dest->data; 
    ptr[0] = size;
    ptr[1] = n_dims;

    return 0;
}

//Convert a binary to an array.
//Expected binary format:
// ------------------------------------------------------------------------------------------------
//|| int array_size  | int shape_size | shape_size_0 | ... | stride_size_0 | ... | array_content ||
//-------------------------------------------------------------------------------------------------
array bin_to_array(ErlNifBinary bin){
    array a;
    int* read_i = (int*) bin.data;
    a.size     = read_i[0];
    a.n_dims   = read_i[1]; 
    a.shape    = read_i + 2;
    a.stride   = read_i + 2 + a.n_dims;
    a.content  = (double*) (a.stride+a.n_dims);
    return a;
}
