#ifndef NIF_MATH_INCLUDED
#define NIF_MATH_INCLUDED


#include<math.h>
#include"nif_utils.h"

typedef struct{
    int size;
    int n_dims;
    int* shape;
    int* stride;
    int stride_r;   //Stride right: used as a mask.
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

    if((error = tupple_to_array(env, argv[0], &lhs))
        || (error = tupple_to_array(env, argv[1], &rhs)))
        return error;

    // Find dimensions of output/output
    dest.n_dims = max(lhs.n_dims, rhs.n_dims);
    dest.size = 1;

    ErlNifBinary shape_bin, stride_bin;
    if(!enif_alloc_binary(sizeof(int)*dest.n_dims, &shape_bin)
        ||!enif_alloc_binary(sizeof(int)*dest.n_dims, &stride_bin)) 
        return exception(env, "Could not allocate shapes.\n");      // MEM LEAK HERE HAHAHA

    dest.shape = (int*) shape_bin.data;
    dest.stride = (int*) stride_bin.data;
    
    for(int i=1; i<=dest.n_dims; i++){
        //Find output strides and dimensions
        int lhs_dim = i<=lhs.n_dims? lhs.shape[lhs.n_dims-i] : 1;
        int rhs_dim = i<=rhs.n_dims? rhs.shape[rhs.n_dims-i] : 1;

        if(lhs_dim != rhs_dim && min(rhs_dim, lhs_dim) != 1)
            return exception(env,"Incompatible shape inputs");
        
        
        int cur_pos = dest.n_dims-i;
        
        dest.shape[cur_pos] = max(lhs_dim, rhs_dim);
        dest.stride[cur_pos] = i==1? 1:dest.shape[cur_pos+1]*dest.stride[cur_pos+1];
    }

    //Allocate destination space.
    ErlNifBinary content_bin;
    if(!enif_alloc_binary(sizeof(double)*dest.stride[0]*dest.shape[0], &content_bin))
        return exception(env, "Could not allocate content");

    //Shift all arrays.
    dest.content = (double*) content_bin.data;
    while(dest.shape[dest.n_dims-1] == 1 && lhs.n_dims > 1 && rhs.n_dims > 1){
        dest.n_dims--;
        lhs.n_dims--;
        rhs.n_dims--;
    }

    if(lhs.shape[lhs.n_dims-1]==1) lhs.stride_r=0; else lhs.stride_r = lhs.stride[lhs.n_dims-1];
    if(rhs.shape[rhs.n_dims-1]==1) rhs.stride_r=0; else rhs.stride_r = rhs.stride[rhs.n_dims-1];
    dest.stride_r = dest.stride[dest.n_dims-1];
    
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


    return enif_make_tuple3(env, enif_make_binary(env, &content_bin), enif_make_binary(env, &shape_bin), enif_make_binary(env, &stride_bin));
}


typedef struct {
    char* name;
    void(*fct)(array,array,array,int);
} BINARY_OP;


void op_add(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++)
        dest.content[i*dest.stride_r] = lhs.content[i*lhs.stride_r] + rhs.content[i*rhs.stride_r];
}

void op_sub(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i*dest.stride[dest.n_dims-1]] = lhs.content[i*lhs.stride[lhs.n_dims-1]] - rhs.content[i*rhs.stride[rhs.n_dims-1]];
    }
}

void op_mult(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i*dest.stride[dest.n_dims-1]] = lhs.content[i*lhs.stride[lhs.n_dims-1]] * rhs.content[i*rhs.stride[rhs.n_dims-1]];
    }
}

void op_div(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i*dest.stride[dest.n_dims-1]] = lhs.content[i*lhs.stride[lhs.n_dims-1]] / rhs.content[i*rhs.stride[rhs.n_dims-1]];
    }
}

void op_pow(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i*dest.stride[dest.n_dims-1]] = pow(lhs.content[i*lhs.stride[lhs.n_dims-1]], rhs.content[i*rhs.stride[rhs.n_dims-1]]);
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
