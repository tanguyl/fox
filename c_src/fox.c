#include "nif_math.h"
#include "nif_utils.h"


ERL_NIF_TERM op(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    char name[254];
    if(!enif_get_string(env, argv[0], name, 253,ERL_NIF_LATIN1))
        return exception(env, "Could not read function name.");

    // Retrieve operation
    UNARY_OP* op;
    for(op = UNARY_OPS; op->name!=""; op++)
        if(!strcmp(name, op->name)) break;
    if(op->name == "")    
        return exception(env, "Could not find operation %s", name);

    // Retrive remeaining inputs
    ERL_NIF_TERM error;
    ErlNifBinary in_bin, out_bin;
    
    if(!enif_inspect_binary(env, argv[1], &in_bin))
        return exception(env,"Could not read array");

    if(!enif_alloc_binary(in_bin.size, &out_bin))
        return exception(env, "Could not allocate binary");

    double* in  = (double*) in_bin.data;
    double* out = (double*) out_bin.data;
    for(int i = 0; i<in_bin.size/sizeof(double); i++)
        out[i] = op->fct(in[i]);
    
    return enif_make_binary(env, &out_bin);
}

ERL_NIF_TERM bin_op(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){

    char name[254];
    if(!enif_get_string(env, argv[0], name, 253,ERL_NIF_LATIN1))
        return exception(env, "Could not read function name.");

    // Retrieve operation
    BINARY_OP* op;
    for(op = BINARY_OPS; op->name!=""; op++)
        if(!strcmp(name, op->name)) break;
    if(op->name == "")    
        return exception(env, "Could not find operation %s", name);
    
    array lhs, rhs, dest;
    ERL_NIF_TERM error;

    if((error = record_to_array(env, argv[1], &dest))
        || (error = record_to_array(env, argv[2], &lhs))
        || (error = record_to_array(env, argv[3], &rhs)))
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
        op->fct(dest, lhs, rhs, iteration_size);

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


ERL_NIF_TERM linspace(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    double start, stop;
    int num;
    ErlNifBinary res_bin;

    if(!enif_get_double(env,argv[0],&start)
        || !enif_get_double(env,argv[1],&stop)
        || !enif_get_int(env, argv[2], &num)
        || !enif_alloc_binary(sizeof(double)*num, &res_bin))
        return enif_make_badarg(env);

    double* dest = (double*) res_bin.data;
    dest[0] = start;
    if (num>1){
        double delta = (stop-start)/((double)num-1);
        int step;
        double val;
        for(step=1,val=start+delta; step<num; step++, val+=delta)
        dest[step] = val;
    }
    
    return enif_make_binary(env, &res_bin);
}

ErlNifFunc nif_funcs[] = { 
    {"op_nif",          2,        op, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"bin_op_nif",      4,    bin_op, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"linspace_nif",    3,  linspace, ERL_NIF_DIRTY_JOB_CPU_BOUND}
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