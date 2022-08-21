#include "nif_math.h"
#include "nif_utils.h"


ERL_NIF_TERM op(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    char name[254];
    if(!enif_get_string(env, argv[0], name, 253,ERL_NIF_LATIN1))
        return exception(env, "Could not read function name.");

    if(argc - 1 == 1){
        //Unary op
        for(UNARY_OP* op = UNARY_OPS; op->name!=""; op++){
            if(!strcmp(name, op->name)){
                return transform(env, argc-1, argv+1, op->fct);
            }
        }
    } else {
        // Binary op
        for(BINARY_OP* op = BINARY_OPS; op->name!=""; op++){
            if(!strcmp(name, op->name)){
                return broadcast(env, argc-1, argv+1, op->fct);
            }
        }
    }
    
    return exception(env, "Could not find operation %s", name);
}

double reduce_op(double* source, int stride, int shape){
    double result=0;
    for(int i = 0; i < shape; i++)
        result += source[stride*i];
    
    return result;
}

ERL_NIF_TERM reduce(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    ErlNifBinary b_in, b_out;
    int stride, shape;

    if(!enif_inspect_binary(env, argv[0], &b_in)
        || !enif_get_int(env, argv[1], &stride)
        || !enif_get_int(env, argv[2], &shape)
        || !enif_alloc_binary(sizeof(double)*stride, &b_out))
        return exception(env, "Could not read inputs");

    double* source = (double*)b_in.data; 
    double* dest   = (double*)b_out.data;

    for(int offset=0; offset<stride; offset++)
        for(int i=0; i<shape; i++)
            dest[offset] = reduce_op(source, stride, shape);

    return enif_make_binary(env, &b_out);
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
    {"op_nif", 2, op},
    {"op_nif", 4, op},
    {"reduce_nif", 3, reduce},
    {"linspace_nif",3, linspace}
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