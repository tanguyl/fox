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
ERL_NIF_TERM tupple_to_array(ErlNifEnv* env, ERL_NIF_TERM term, array* dest){
    int arity;
    ERL_NIF_TERM* terms;

    if(!enif_get_tuple(env, term, &arity, &terms) || arity<3)
        return exception(env,"Could not read array");
    
    ErlNifBinary bin;

    if(!enif_inspect_binary(env, terms[0], &bin)) return exception(env, "Could not read content.");
    dest->content = bin.data;
    dest->size = bin.size/sizeof(double);

    if(!enif_inspect_binary(env, terms[1], &bin)) return exception(env, "Could not read shapes.");
    dest->shape = bin.data;
    dest->n_dims = bin.size/sizeof(int);
    dest->shape_r = dest->shape[dest->n_dims-1];

    if(!enif_inspect_binary(env, terms[2], &bin)) return exception(env, "Coule not read strides.");
    dest->stride = bin.data;
    
    return 0;
}


ERL_NIF_TERM transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv, double(*fct)(double)){
    ERL_NIF_TERM error;
    ErlNifBinary in_bin, out_bin;
    
    if(!enif_inspect_binary(env, argv[0], &in_bin))
        return exception(env,"Could not read array");

    if(!enif_alloc_binary(in_bin.size, &out_bin))
        return exception(env, "Could not allocate binary");

    double* in  = (double*) in_bin.data;
    double* out = (double*) out_bin.data;
    for(int i = 0; i<in_bin.size/sizeof(double); i++)
        out[i] = fct(in[i]);
    
    return enif_make_binary(env, &out_bin);
}


typedef struct {
    char* name;
    double(*fct)(double);    
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

ERL_NIF_TERM broadcast(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv, void(*fct)(array,array,array,int)){
    
    array lhs, rhs, dest;
    ERL_NIF_TERM error;

    if((error = tupple_to_array(env, argv[0], &dest))
        || (error = tupple_to_array(env, argv[1], &lhs))
        || (error = tupple_to_array(env, argv[2], &rhs)))
        return error;

    //Allocate destination space.
    ErlNifBinary content_bin;
    if(!enif_alloc_binary(sizeof(double)*dest.stride[0]*dest.shape[0], &content_bin))
        return exception(env, "Could not allocate content");
    
    dest.content = (double*) content_bin.data;

    int iteration_size = dest.shape[dest.n_dims-1];
    int cur_it[dest.n_dims];
    memset(cur_it, 0, sizeof(int)*dest.n_dims);
    int iterate = 1;

    array lhs_base = lhs, rhs_base = rhs, dest_base = dest;

    while(iterate){
        debug_write("Iterating! Is size is %i\n", iteration_size);
        fct(lhs, rhs, dest, iteration_size);

        //Apply iteration factor
        if(dest.n_dims==1)
            iterate = 0;
        else{
            cur_it[dest.n_dims-1] += iteration_size;
            for(int i = dest.n_dims-1; i>=0; i--)
                if(cur_it[i] >= dest.shape[i]){
                    if(i==0)
                        iterate = 0;
                    else{
                        cur_it[i]    = 0;
                        cur_it[i-1] += 1;
                    }
                }
            
            // shift arrays
            lhs = shift_array(lhs_base, cur_it, dest.n_dims);
            rhs = shift_array(rhs_base, cur_it, dest.n_dims);
            dest = shift_array(dest_base, cur_it, dest.n_dims);
        }
        
    }

    return enif_make_binary(env, &content_bin);
}


typedef struct {
    char* name;
    void(*fct)(array,array,array,int);
} BINARY_OP;


void op_add(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++)
        dest.content[i] = lhs.content[i%lhs.shape_r] + rhs.content[i%rhs.shape_r];
}

void op_sub(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i] = lhs.content[i%lhs.shape_r] - rhs.content[i%rhs.shape_r];
    }
}

void op_mult(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i] = lhs.content[i%lhs.shape_r] * rhs.content[i%rhs.shape_r];
    }
}

void op_div(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i] = lhs.content[i%lhs.shape_r] / rhs.content[i%rhs.shape_r];
    }
}

void op_pow(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i] = pow(lhs.content[i%lhs.shape_r], rhs.content[i%rhs.shape_r]);
    }
}

BINARY_OP BINARY_OPS[] = {
    {"add", op_add},
    {"sub", op_sub},
    {"mult", op_mult},
    {"div", op_div},
    {"pow", op_pow},
    {"", NULL}
};


#endif
