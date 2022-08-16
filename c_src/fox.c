#include "nif_math.h"
#include "nif_utils.h"


ERL_NIF_TERM op(ErlNifEnv* env, int argc, const ERL_NIF_TERM* argv){
    char name[254];
    if(!enif_get_string(env, argv[0], name, 253,ERL_NIF_LATIN1))
        return exception(env, "Could not read function name.");

    debug_write("Red operation.\n");

    for(UNARY_OP* op = UNARY_OPS; op->name!=""; op++){
        debug_write("Searching.\n");
        if(!strcmp(name, op->name)){
            debug_write("In operation...\n");
            debug_write("Found operation, %s(0) = %lf\n", op->name, op->fct(0.0));
            return transform(env, argc-1, argv+1, op->fct);
        }
    }
    
    return exception(env, "Could not find operation %s", name);
}
   


ErlNifFunc nif_funcs[] = {{"op", 2, op}};

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