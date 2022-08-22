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
    int stride_r;
    double* content;
} array;

typedef struct{
    int*  cur_it;
    int*  limits;
    int   n_dims;
    int   iterating;
    int   it_size;
} iterator;

iterator alloc_iterator(array iterated, int it_size){
    iterator it;
    it.it_size = it_size;
    it.limits  = iterated.shape;
    it.n_dims  = iterated.n_dims;
    it.iterating = 1;

    it.cur_it  = enif_alloc(sizeof(int)*iterated.n_dims);
    memset(it.cur_it, 0, sizeof(int)*it.n_dims);

    return it;
}

void free_iterator(iterator* it){
    enif_free(it->cur_it);
    it->cur_it = NULL;
    it->limits = NULL;
}

void iterate(iterator* it){
    if(it->n_dims==1)
        it->iterating = 0;
    else{
        it->cur_it[it->n_dims-1] += it->it_size;
        for(int i = it->n_dims-1; i>=0; i--)
            if(it->cur_it[i] >= it->limits[i]){
                if(i==0)
                    it->iterating = 0;
                else{
                    it->cur_it[i]    = 0;
                    it->cur_it[i-1] += 1;
                }
            }
    }
}

array shift_array(array a, iterator it){
    int shift = 0;
    for(int current=1; current <= a.n_dims; current++){
        shift += (a.shape[a.n_dims-current] == 1? 0:a.stride[a.n_dims-current]) * it.cur_it[it.n_dims-current];
    }
    a.content += shift;
    return a;
}

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
    dest->stride_r = dest->stride[dest->n_dims-1];
    
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

typedef struct {
    char* name;
    void(*fct)(array, array, array, int);
} BINARY_OP;


void op_add(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i] = lhs.content[i%lhs.shape_r] + rhs.content[i%rhs.shape_r];
    }
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
