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
  int number;
  char *value;
  char *desc;
  ERL_NIF_TERM atom;
};

static ERL_NIF_TERM atom_undefined;

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
  int ival = 0;
  double dval = 0.0;
  double coeff = 0.0;
  
  ERL_NIF_TERM *reply;
  int reply_capacity, reply_size;
  
  reply_capacity = 16;
  reply = (ERL_NIF_TERM *)calloc(sizeof(ERL_NIF_TERM), reply_capacity);
  reply_size = 0;
  
  int next_data_length = -1;
  
  // fprintf(stderr, "Splitting %.*s\r\n", 20, (char *)ptr);
  
  for(; ptr < input.data + input.size; ) {
    switch(state) {
      case READING_CODE:
        while(*ptr >= '0' && *ptr <= '9' && ptr < end) {
          ival = ival*10 + *ptr - '0';
          ptr++;
        }
        if(ptr < end && *ptr == '=') {
          code = ival;
          ival = 0;
          if(is_code(INT_CODE, code)) {
            state = READING_INT;
            dval = 0.0;
            coeff = 0.0;
          } else if(is_code(BOOL_CODE, code)) {
            state = READING_BOOL;
          } else {
            state = READING_STRING;
          }
          // fprintf(stderr, "Message %d\r\n", code);
          
          if(reply_size >= reply_capacity - 2) {
            reply_capacity *= 2;
            reply = (ERL_NIF_TERM *)realloc(reply, reply_capacity*sizeof(ERL_NIF_TERM));
          }
          
          ptr++;
          continue;
        }
        //FIXME: clean reply
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
        ERL_NIF_TERM value;
        if(state == READING_INT) {
          // fprintf(stderr, "Read int %s = %d\r\n", FIELD_NAMES[code], ival);
          value = enif_make_int(env, ival);
          
          if(is_code(LENGTH_CODE, code)) {
            next_data_length = ival;
          }
        } else if (state == READING_DOUBLE) {
          // fprintf(stderr, "Read double %s = %.2f\r\n", FIELD_NAMES[code], dval);
          value = enif_make_double(env, dval);
        } else {
          //FIXME: clean reply
          return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, ptr - input.data));
        }
        // fprintf(stderr, "Read int part of value: %d, %d\r\n", ival, *ptr);
        
        ival = 0;
        dval = 0.0;
        coeff = 0.0;
        if(ptr < end && *ptr == 1) {
          if(!is_code(LENGTH_CODE, code)) {
            reply[reply_size++] = enif_make_tuple2(env, FIELD_ATOMS[code], value);
          }
          state = READING_CODE;
          code = 0;
          ptr++;
          continue;
        }
        //FIXME: clean reply
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, ptr - input.data));
      }
      
      case READING_STRING: {
        unsigned char *string_begin = ptr;
        
        if(next_data_length >= 0) {
          ptr += next_data_length;
          if(ptr >= end || *ptr != 1) {
            //FIXME: clean reply
            return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, ptr - input.data));
          }
          next_data_length = -1;
        } else {
          while(ptr < end && *ptr != 1) {
            ptr++;
          }
        }
        
        if(ptr == end) {
          //FIXME: clean reply
          return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, ptr - input.data));
        }
        
        ERL_NIF_TERM value = atom_undefined;
        if(is_code(CHOICE_CODE, code)) {
          int i;
          for(i = 0; CHOICE_VALUES[i].value; i++) {
            if(CHOICE_VALUES[i].number == code && !strncmp(CHOICE_VALUES[i].value, (const char *)string_begin, ptr - string_begin)) {
              value = CHOICE_VALUES[i].atom;
              break;
            }
          }
        }
        if(value == atom_undefined) {
          ErlNifBinary bin;
          enif_alloc_binary(ptr - string_begin, &bin);
          memcpy(bin.data, string_begin, ptr - string_begin);
          value = enif_make_binary(env, &bin);
        }
        
        reply[reply_size++] = enif_make_tuple2(env, FIELD_ATOMS[code], value);
        
        // fprintf(stderr, "Read string %s -> '%.*s'\r\n", FIELD_NAMES[code], (int)(ptr - string_begin), string_begin);
        state = READING_CODE;
        code = 0;
        ptr++;
        continue;
      }
      
    }
  }
  
  ERL_NIF_TERM list = enif_make_list_from_array(env, reply, reply_size);
  free(reply);
  return list;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  int i;
  for(i = 0; i < sizeof(FIELD_ATOMS) / sizeof(*FIELD_ATOMS); i++) {
    FIELD_ATOMS[i] = enif_make_atom(env, FIELD_NAMES[i]);
  }
  
  for(i = 0; CHOICE_VALUES[i].desc; i++) {
    CHOICE_VALUES[i].atom = enif_make_atom(env, CHOICE_VALUES[i].desc);
  }
  
  atom_undefined = enif_make_atom(env, "undefined");
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
