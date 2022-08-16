#include<math.h>

typedef struct{
    int size;
    int n_dims;
    int* shape;
    int* stride;
    double* content;
} array;


//Represent a broadcasting iterator.
typedef struct{
    array lhs, rhs, dest;
    array iterated_lhs, iterated_rhs, iterated_dest;

    int n_dims;
    int* cur_it;
    int* it_lim;
    int* masked_strides;
    int it_size;

} array_it;


//Some utility functions.
// Found at https://stackoverflow.com/questions/3437404/min-and-max-in-c.
#define max(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

#define min(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })


//Create an arary iterator
array_it* alloc_iterator(array lhs, array rhs, array dest){
    array_it *ait = enif_alloc(sizeof(array_it));
    while(dest.n_dims > 1 && dest.shape[dest.n_dims-1] == 1 && min(lhs.n_dims, rhs.n_dims) != 1){
        lhs.n_dims--;
        rhs.n_dims--;
        dest.n_dims--;
    }
    ait->lhs = lhs;
    ait->rhs = rhs;
    ait->dest = dest;

    ait->n_dims = dest.n_dims;
    ait->cur_it  = enif_alloc(sizeof(int)*ait->n_dims);
    memset(ait->cur_it, 0, sizeof(int)*ait->n_dims);
    ait->it_lim = dest.shape;
    ait->cur_it[ait->n_dims-1] = -ait->it_lim[ait->n_dims-1];
    ait->it_size = dest.shape[dest.n_dims-1];

    //Mask strides 
    ait->masked_strides = NULL;
    if(ait->it_size != 1){
        array *smoll = (ait->lhs.shape[ait->lhs.n_dims-1] < ait->rhs.shape[ait->rhs.n_dims-1])? &ait->lhs:&ait->rhs;

        if (smoll->shape[smoll->n_dims-1] == 1){
            ait->masked_strides = enif_alloc(sizeof(int)*smoll->n_dims);
            memcpy(ait->masked_strides, smoll->stride, sizeof(int)*smoll->n_dims);
            smoll->stride = ait->masked_strides;
            smoll->stride[smoll->n_dims-1] = 0;
        }
    }
    return ait;
}

void free_iterator(array_it *ait){
    enif_free(ait->cur_it);
    if(ait->masked_strides){
        enif_free(ait->masked_strides);
    }
    enif_free(ait);
}

array array_at(array a, int* index, int index_n_dims){
    int shift = 0;
    for(int current=1; current <= a.n_dims; current++){
        shift += (a.shape[a.n_dims-current] == 1? 0:a.stride[a.n_dims-current]) * index[index_n_dims-current];
    }
    a.content += shift;
    return a;
}


int iterate(array_it *it){
    // Returns the number of elements that can be operated on before calling again iterate
    // if return value == 0: iteration is finished.
    if(it->n_dims >= 1){
        it->cur_it[it->n_dims-1] += it->it_lim[it->n_dims-1];
        for(int i = it->n_dims-1; i>=0; i--){
            if(it->cur_it[i] >= it->it_lim[i]){
                if(i==0){
                    //Finished iterating
                    return 0;
                }
                it->cur_it[i]    = 0;
                it->cur_it[i-1] += 1;
            }
            else break;
        }

        it->iterated_lhs = array_at(it->lhs, it->cur_it, it->n_dims);
        it->iterated_rhs = array_at(it->rhs, it->cur_it, it->n_dims);
        it->iterated_dest = array_at(it->dest, it->cur_it, it->n_dims);
        return 1;
    }
    else return 0;
}




void op_add(array lhs, array rhs, array dest, int n_elem){
    for(int i = 0; i<n_elem; i++){
        dest.content[i*dest.stride[dest.n_dims-1]] = lhs.content[i*lhs.stride[lhs.n_dims-1]] + rhs.content[i*rhs.stride[rhs.n_dims-1]];
    }
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

typedef struct {
    char* name;
    void(*fct)(array, array, array, int);
}op;

op operations[] = {
    {"+", op_add},
    {"-", op_sub},
    {"*", op_mult},
    {"/", op_div},
    {"^", op_pow},
    {"", NULL}
};


op find_op(char* op_str){
    op* op_found;
    for(op_found = operations; strcmp(op_found->name, ""); op_found++){
        if(strcmp(op_found->name, op_str) == 0)
            break;
    }
    return *op_found;
}
