
ERL_NIF_TERM apply_op(ErlNifEnv env, int argc, ERL_NIF_TERM argv[]){
    op sel_op;
    iterator it;
    ERL_NIF_TERM error;

    if(error=find_op(argv[0], &sel_op) 
        || error = sel_op.alloc_set_iterator(argv+1, &it)
        || error = alloc_set_array(it.dest.n_dims, it.dest.n_elems, &it.dest))
        return error;
    
    while(it.iterate())
        sel_op.apply(it);

    Array* dest = &it.dest;
    free_iterator(it);
    return array_to_erlang(env, dest);
}

