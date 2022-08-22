#ifndef NIF_MATH_INCLUDED
#define NIF_MATH_INCLUDED


#include<math.h>
#include"nif_utils.h"

typedef struct{
    int size;
    int n_dims;
    int* shape;
    int* stride;
    int shape_r;
    double* content;
} array;

//Read an erlang tupple into an array represenatation.
ERL_NIF_TERM record_to_array(ErlNifEnv* env, ERL_NIF_TERM term, array* dest){
    int arity;
    ERL_NIF_TERM* terms;

    if(!enif_get_tuple(env, term, &arity, &terms) || arity != 4)
        return exception(env,"Could not read array");
    
    ErlNifBinary bin;

    if(!enif_inspect_binary(env, terms[1], &bin)) return exception(env, "Could not read content.");
    dest->content = bin.data;
    dest->size = bin.size/sizeof(double);

    if(!enif_inspect_binary(env, terms[2], &bin)) return exception(env, "Could not read shapes.");
    dest->shape = bin.data;
    dest->n_dims = bin.size/sizeof(int);
    dest->shape_r = dest->shape[dest->n_dims-1];

    if(!enif_inspect_binary(env, terms[3], &bin)) return exception(env, "Could not read strides.");
    dest->stride = bin.data;
    
    return 0;
}

typedef struct {
    char* name;
    double (*fct)(double);    
} UNARY_OP;

UNARY_OP UNARY_OPS[] = {
    {"acos", acos}, 
    {"acosh", acosh},
    {"asin", asin},
    {"asinh", asinh},
    {"atan", atan},
    {"atan2", atan2},
    {"atanh", atanh},
    {"ceil", ceil},
    {"cos", cos},
    {"cosh", cosh},
    {"exp", exp},
    {"floor", floor},
    {"fmod", fmod},
    {"log", log},
    {"log10", log10},
    {"log2", log2},
    {"sin", sin},
    {"sinh", sin},
    {"sqrt", sqrt},
    {"tan", tan},
    {"tanh", tanh},
    {"", NULL}
};


array shift_array(array a, int* index, int index_n_dims){
    int shift = 0;
    for(int current=1; current <= a.n_dims; current++){
        shift += (a.shape[a.n_dims-current] == 1? 0:a.stride[a.n_dims-current]) * index[index_n_dims-current];
    }
    a.content += shift;
    return a;
}

typedef struct {
    char* name;
    void(*fct)(array, array, array, int);
} BINARY_OP;


void op_add(array dest, array lhs, array rhs, int n_elem){
    debug_write("Operating! it size is %i\n", n_elem);
    for(int i = 0; i<n_elem; i++){
        dest.content[i] = lhs.content[i%lhs.shape_r] + rhs.content[i%rhs.shape_r];
    }
}

BINARY_OP BINARY_OPS[] = {
    {"add", op_add},
    {"", NULL}
};

#endif
