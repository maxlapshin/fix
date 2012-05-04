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

struct ValueDesc {
  int code;
  char *value;
  char *desc;
  ERL_NIF_TERM atom;
};

#include "splitter.h"

enum {READING_CODE, READING_INT, READING_DOUBLE, READING_BOOL, READING_STRING};

enum CODE_TYPE {INT_CODE, BOOL_CODE, LENGTH_CODE, CHOICE_CODE} ;

static inline int is_code(enum CODE_TYPE type, int code) {
  unsigned char *table;
  switch(type) {
    case INT_CODE:    table = INT_CODES; break;
    case BOOL_CODE:   table = BOOL_CODES; break;
    case LENGTH_CODE: table = LENGTH_CODES; break;
    case CHOICE_CODE: table = CHOICE_CODES; break;
    default: fprintf(stderr, "Invalid code type: %d\r\n", type); exit(1);
  }
  
  return (code / 8 > sizeof(INT_CODES)) ? 0 : (table[code / 8] >> (7 - (code % 8))) & 1;
}
// #define IS_CODE(TABLE, X) ((X / 8 > sizeof((TABLE)_CODES)) ? 0 : (TABLE)_CODES[X / 8] >> (8 - X % 8))

static ERL_NIF_TERM
split(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary input;
  if(!enif_inspect_binary(env, argv[0], &input)) return enif_make_badarg(env);
  
  unsigned char *ptr = input.data;
  unsigned char *end = input.data + input.size;
  
  int state = READING_CODE;
  int code = 0;
  int ival;
  double dval;
  double coeff;
  
  for(; ptr < input.data + input.size; ) {
    switch(state) {
      case READING_CODE:
        while(*ptr >= '0' && *ptr <= '9' && ptr < end) {
          code = code*10 + *ptr - '0';
          ptr++;
        }
        if(ptr < end && *ptr == '=') {
          if(is_code(INT_CODE, code)) {
            state = READING_INT;
            ival = 0;
            dval = 0;
            coeff = 0.0;
          } else if(is_code(BOOL_CODE, code)) {
            state = READING_BOOL;
          } else {
            state = READING_STRING;
          }
          fprintf(stderr, "Message %d %s\r\n", code, FIELD_NAMES[code]);
          ptr++;
          continue;
        }
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, ptr - input.data));
      
      case READING_INT: {
        while(*ptr >= '0' && *ptr <= '9' && ptr < end) {
          ival = ival*10 + *ptr - '0';
          ptr++;
        }
        if(ptr < end && *ptr == '.') {
          state = READING_DOUBLE;
          ptr++;
          dval = ival;

          while(*ptr >= '0' && *ptr <= '9' && ptr < end) {
            coeff = coeff*0.1;
            dval = dval + (*ptr - '0')*coeff;
            ptr++;
          }
        }
        if(state == READING_INT) {
          fprintf(stderr, "Read int %s = %d\r\n", FIELD_NAMES[code], ival);
        } else {
          fprintf(stderr, "Read double %s = %.2f\r\n", FIELD_NAMES[code], dval);
        }
        if(ptr < end && *ptr == 1) {
          state = READING_CODE;
          ptr++;
          continue;
        }
        
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, ptr - input.data));        
      }
      
      case READING_STRING: {
        unsigned char *string_begin = ptr;
        while(ptr < end && *ptr != 1) {
          ptr++;
        }
        
        fprintf(stderr, "Read string %s -> '%.*s'\r\n", FIELD_NAMES[code], (int)(ptr - string_begin), string_begin);
        ptr++;
        continue;
      }
      
    }
  }
  
  return argv[0];
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  int i;
  for(i = 0; i < sizeof(FIELD_ATOMS) / sizeof(*FIELD_ATOMS); i++) {
    FIELD_ATOMS[i] = enif_make_atom(env, FIELD_NAMES[i]);
  }
  
  for(i = 0; CHOICE_VALUES[i].desc; i++) {
    CHOICE_VALUES[i].atom = enif_make_atom(env, CHOICE_VALUES[i].desc);
  }
  return 0;
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


ERL_NIF_INIT(fix_splitter, fix_splitter_funcs, load, reload, upgrade, NULL)
