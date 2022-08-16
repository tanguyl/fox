#include "nif_utils.h"

/*
Arity: 2
Inputs:     List of ints/doubles (content), list of shapes.
Outputs:    A binary version of the array.
*/
ERL_NIF_TERM nif_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    ERL_NIF_TERM list = argv[0];
    
    //Read size/shape
    ERL_NIF_TERM elem; 
    int n_dims, n_content;
    if(!enif_get_list_cell(env, list, &elem, &list))        return exception(env, "Could not read array size.");            // Get size of array
    if(!enif_get_int(env, elem, &n_content))                return exception(env, "Could not write array size.");           // Save it.
    if(!enif_get_list_cell(env, list, &elem, &list))        return exception(env, "Could not read array shape's size.");    // Get number of dimensions
    if(!enif_get_int(env, elem, &n_dims))                   return exception(env, "Could not write array shape's size.");   // Save it.

    if(!n_content || !n_dims)                               return exception(env, "Array contains %i elements and %i dimensions; should be non zero.", n_content, n_dims);

    ErlNifBinary array_bin;
    ERL_NIF_TERM error = 0;
    if (error=alloc_array_binary(env, &array_bin, n_content, n_dims))
                                                            return error;
     
    //Write the shape.
    int* dest = (int*)array_bin.data;
    *(dest++) = n_content;
    *(dest++) = n_dims;
    error = 0;
    int n_expected = 1;

    for(int i = 0; i<n_dims; i++){
        if(!enif_get_list_cell(env, list, &elem, &list))    error = exception(env, "Could not read the %i shape coordinate", i);
        else 
        if(!enif_get_int(env, elem, dest))                  error = exception(env, "Could not write the %i shape coordinate", i);

        if(error){
            enif_release_binary(&array_bin);
            return error;
        }
        n_expected *= *dest;
        dest++;
    }
    if(n_expected != n_content)                             return exception(env, "Shape mismatch with array content: asked shape contains %i elements, array contains %i.", n_expected, n_content);

    //Write the damn strides.
    dest[n_dims-1]=1;
    for(int i =n_dims-2; i>=0; i--){
        dest[i] = dest[i-n_dims+1] * dest[i+1];
    }
    
    //Finally: read content. 
    double* dest_d = (double*) (dest+n_dims);
    int translated, it=0;
    while (enif_get_list_cell(env, list, &elem, &list)){
        if(!enif_get_double(env, elem, dest_d + it)){
            if(enif_get_int(env, elem, &translated))
                dest_d[it] = (double) translated;
            else {
                enif_release_binary(&array_bin);            return exception(env, "Could not read the element at position %i: incorrect format.", it);
            }
        }
        it++;
    }
    
    return enif_make_binary(env, &array_bin);
}

ERL_NIF_TERM nif_clean_apply_op(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    op sel_op;
    array_it it;
    ERL_NIF_TERM error;

    if(error=find_op(argv[0], &sel_op) 
        || error = sel_op.alloc_set_iterator(argv+1, &it)
        || error = alloc_set_array(it.dest.n_dims, it.dest.size, &it.dest))
        return error;
    
    while(it.iterate())
        sel_op.apply(it);

    array* dest = &it.dest;
    free_iterator(it);
    return array_to_erlang(env, dest);
}

/*
Input: "small" array.
Output: an atom representation of the array: [array_len n_dims shape0 ... stride0 ... content 0 ...]
*/
ERL_NIF_TERM nif_array_to_atom(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    ErlNifBinary bin;
    if(!enif_inspect_binary(env, argv[0], &bin))            return exception(env, "Input should be a binary containing an array.");

    array a = bin_to_array(bin);
    char result[1000000];
    int written = 0;
    

    written += sprintf(result + written, "size: %i, n_dims: %i, shape: {", a.size, a.n_dims);
    for(int i = 0; i<a.n_dims; i++)
        written += sprintf(result + written, "%i ", a.shape[i]);
    written += sprintf(result + written, "}, stride: {");
    for(int i = 0; i<a.n_dims; i++)
        written += sprintf(result + written, "%i ", a.stride[i]);
    written += sprintf(result + written, "}, content: {");
    for(int i = 0; i<a.size; i++)        
        written += sprintf(result + written, "%.2lf ", a.content[i]);
    written += sprintf(result + written, "}");


    return enif_make_string(env, result, ERL_NIF_LATIN1);
}


ERL_NIF_TERM apply_op(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){

    ErlNifBinary lhs_bin, rhs_bin;
    char op_str[2];

    if(!enif_get_atom(env, argv[0], op_str, 2, ERL_NIF_LATIN1))
                                                            return exception(env, "Invalid input: could not read operation.");
    op operation = find_op(op_str);
    if(!operation.fct){
                                                            return exception(env, "Failed to read operation.");
    }
    if(!enif_inspect_binary(env, argv[1], &lhs_bin) || ! enif_inspect_binary(env, argv[2], &rhs_bin))
                                                            return exception(env, "Invalid inputs: argv 1,2 should be binaries (arrays)");

    array lhs = bin_to_array(lhs_bin), rhs = bin_to_array(rhs_bin);   

    //Determine shape of resulting array
    int n_dims = max(lhs.n_dims, rhs.n_dims);
    int shape[n_dims];
    memset(shape, 0, sizeof(int)*n_dims);


    for(int ai = 0; ai<2; ai++){
        array a = ai==0? lhs:rhs;
        for(int i =1; i<=a.n_dims; i++){
            if(!shape[n_dims-i])    shape[n_dims-i] = a.shape[a.n_dims-i];
            else                    shape[n_dims-i] = max(shape[n_dims-i], a.shape[a.n_dims-i]);   
        }
    }

    int size = 1;
    for(int i = 0; i<n_dims; i++){
        if(__INT_MAX__ / size < shape[i])
            return exception(env, "Overflow: output array cannot be represented.");
        else
            size *= shape[i];
    }
    

    ErlNifBinary result_bin;
    ERL_NIF_TERM error = 0;
    if(error = alloc_array_binary(env, &result_bin, size, n_dims))
                                                            return error;

    
    array dest = bin_to_array(result_bin);
    dest.stride[n_dims-1]=1;
    memcpy(dest.shape, shape, sizeof(int)*n_dims);
    memset(dest.content, 0, sizeof(double)*size);

    // Set strides.
    for(int i =n_dims-2; i>=0; i--){
        dest.stride[i] = dest.stride[i-n_dims+1] * dest.stride[i+1];
    }

    
    array_it *it = alloc_iterator(lhs, rhs, dest);
    while(iterate(it)){
        operation.fct(it->iterated_lhs, it->iterated_rhs, it->iterated_dest, it->it_size);
    }

    free_iterator(it);
    return enif_make_binary(env, &result_bin);
}

ErlNifFunc nif_funcs[] = {
    {"build_array", 1, nif_array},
    {"array_to_atom", 1, nif_array_to_atom},
    {"apply_nif", 3, apply_op}
};

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info){
    debug_write("\nNew session\n-----------\n");
    return 0;
}

int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info){
    return 0;
}

int unload(ErlNifEnv* caller_env, void* priv_data){
    return 0;
}

ERL_NIF_INIT(fox, nif_funcs, load, NULL, upgrade, unload)