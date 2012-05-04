#include <stdint.h>
#include "erl_nif.h"
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>

#include "splitter.h"

enum {READING_CODE, READING_VALUE};



static ERL_NIF_TERM
split(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary input;
  if(!enif_inspect_binary(env, argv[0], &input)) return enif_make_badarg(env);
  
  unsigned char *ptr = input.data;
  unsigned char *end = input.data + input.size;
  
  int state = READING_CODE;
  int code = 0;
  
  for(; ptr < input.data + input.size; ) {
    switch(state) {
      case READING_CODE:
        while(*ptr >= '0' && *ptr <= '9' && ptr < end) {
          code = code*10 + *ptr - '0';
          ptr++;
        }
        if(ptr < end && *ptr == '=') {
          
          state = READING_VALUE;
          ptr++;
          continue;
        }
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, ptr - input.data));
      
      case READING_VALUE: {
        
        continue;
      }
      
    }
  }
  
  return argv[0];
}


static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

static ErlNifFunc fix_splitter_funcs[] =
{
  {"split", 1, split}
};


ERL_NIF_INIT(fix_splitter, fix_splitter_funcs, NULL, reload, upgrade, NULL)
