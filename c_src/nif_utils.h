#ifndef NIF_UTILS_INCLUDED
#define NIF_UTILS_INCLUDED

#include "erl_nif.h"
#include <stdio.h>
#include <string.h>

//Used for debug purpose.
//Likely thread unsafe.
//Usage: debug_write("A double: %lf, an int:%d", double_val, int_val);
int debug_write(const char* fmt, ...){
    FILE* fp = fopen("debug.txt", "a");
    va_list args;

    va_start(args, fmt);
    vfprintf(fp, fmt, args);
    va_end(args);

    fclose(fp);
    return 1;
}

//Create an exception term.
ERL_NIF_TERM exception(ErlNifEnv* env, const char* format, ...){
    char error[254];

    va_list arg;
    va_start(arg, format);
    vsnprintf(error, 250, format, arg);
    va_end(arg);

    return enif_raise_exception(env, enif_make_atom(env, error));
}


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




#endif