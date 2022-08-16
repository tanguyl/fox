#ifndef NIF_MATH_INCLUDED
#define NIF_MATH_INCLUDED


#include<math.h>
#include"nif_utils.h"

typedef struct{
    int size;
    int n_dims;
    int* shape;
    int* stride;
    double* content;
} array;


typedef struct {
    char* name;
    double(*fct)(double);    
} UNARY_OP;


//Read an erlang tupple into an array represenatation.
ERL_NIF_TERM tupple_to_array(ErlNifEnv* env, ERL_NIF_TERM term, array* dest){
    int arity;
    ERL_NIF_TERM* terms;

    if(!enif_get_tuple(env, term, &arity, &terms) || arity<3)
        return exception(env,"Could not read array");
    
    ErlNifBinary bin;

    if(!enif_inspect_binary(env, terms[0], &bin)) return exception(env, "Could not read content.");
    dest->content = bin.data;
    dest->size = bin.size/sizeof(float);

    if(!enif_inspect_binary(env, terms[1], &bin)) return exception(env, "Could not read shapes.");
    dest->shape = bin.data;
    dest->n_dims = bin.size/sizeof(float);

    if(!enif_inspect_binary(env, terms[2], &bin)) return exception(env, "Coule not read strides.");
    dest->stride = bin.data;
    
    return 0;
}


ERL_NIF_TERM transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv, double(*fct)(double)){
    ERL_NIF_TERM error;
    ERL_NIF_TERM *bins;
    int arity;
    
    if(!enif_get_tuple(env, argv[0], &arity, &bins) || arity<3)
        return exception(env,"Could not read array");

    ErlNifBinary in_bin, out_bin;
    if(!enif_inspect_binary(env, bins[0], &in_bin) || !enif_alloc_binary(in_bin.size, &out_bin))
        return exception(env, "Could not allocate binary");

    double* in = (double*) in_bin.data;
    double* out = (double*) out_bin.data;
    for(int i = 0; i<in_bin.size/sizeof(double); i++)
        out[i] = fct(in[i]);
    
    ERL_NIF_TERM r_term = enif_make_binary(env, &out_bin);
    return enif_make_tuple3(env, r_term, bins[1], bins[2]);
}


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

#endif
