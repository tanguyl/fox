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


ErlNifFunc nif_funcs[] = { {"op_nif", 2, op}, {"op_nif", 3, op}};

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