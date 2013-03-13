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
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

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
  int sign = 1;
  double dval = 0.0;
  double coeff = 1.0;
  
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
            coeff = 1.0;
            sign = 1;
          } else if(is_code(BOOL_CODE, code)) {
            state = READING_BOOL;
          } else {
            state = READING_STRING;
          }
          // fprintf(stderr, "Message %d, %s\r\n", code, FIELD_NAMES[code]);
          
          if(reply_size >= reply_capacity - 2) {
            reply_capacity *= 2;
            reply = (ERL_NIF_TERM *)realloc(reply, reply_capacity*sizeof(ERL_NIF_TERM));
          }
          
          ptr++;
          continue;
        }
        //FIXME: clean reply
        return enif_make_tuple3(env, enif_make_atom(env, "error"), enif_make_atom(env, "reading_code"), enif_make_int(env, ptr - input.data));
      
      case READING_INT: {
        if(ptr < end && *ptr == '-') {
          sign = -1;
          ptr++;
        }
        while(*ptr >= '0' && *ptr <= '9' && ptr < end) {
          ival = ival*10 + *ptr - '0';
          ptr++;
        }
        if(ptr < end && *ptr == '.') {
          state = READING_DOUBLE;
          ptr++;
          dval = 1.0*ival;

          while(ptr < end && *ptr >= '0' && *ptr <= '9') {
            coeff = coeff*0.1;
            dval += (*ptr - '0')*coeff;
            ptr++;
          }
        }
        ERL_NIF_TERM value;
        if(state == READING_INT) {
          // fprintf(stderr, "Read int %s = %d\r\n", FIELD_NAMES[code], ival);
          value = enif_make_int(env, ival*sign);
          
          if(is_code(LENGTH_CODE, code)) {
            next_data_length = ival;
          }
        } else if (state == READING_DOUBLE) {
          // fprintf(stderr, "Read double %s = %.2f\r\n", FIELD_NAMES[code], dval);
          value = enif_make_double(env, dval*sign);
        } else {
          //FIXME: clean reply
          return enif_make_tuple3(env, enif_make_atom(env, "error"), enif_make_atom(env, "reading_int"), enif_make_int(env, ptr - input.data));
        }
        // fprintf(stderr, "Read int part of value: %d, %d\r\n", ival, *ptr);
        
        ival = 0;
        dval = 0.0;
        coeff = 1.0;
        sign = 1;
        if(ptr < end && *ptr == 1) {
          if(!is_code(LENGTH_CODE, code)) {
            reply[reply_size++] = enif_make_tuple2(env, code < MAX_FIELD_NUMBER ? FIELD_ATOMS[code] : enif_make_int(env, code), value);
          }
          state = READING_CODE;
          code = 0;
          ptr++;
          continue;
        }
        //FIXME: clean reply
        return enif_make_tuple3(env, enif_make_atom(env, "error"), enif_make_atom(env, "reading_int_soh"), enif_make_int(env, ptr - input.data));
      }
      
      case READING_STRING: {
        unsigned char *string_begin = ptr;
        
        if(next_data_length >= 0) {
          ptr += next_data_length;
          if(ptr >= end || *ptr != 1) {
            //FIXME: clean reply
            return enif_make_tuple3(env, enif_make_atom(env, "error"), enif_make_atom(env, "reading_string"), enif_make_int(env, ptr - input.data));
          }
          next_data_length = -1;
        } else {
          while(ptr < end && *ptr != 1) {
            ptr++;
          }
        }
        
        if(ptr == end) {
          //FIXME: clean reply
          return enif_make_tuple3(env, enif_make_atom(env, "error"), enif_make_atom(env, "reading_string_soh"), enif_make_int(env, ptr - input.data));
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
        
        reply[reply_size++] = enif_make_tuple2(env, code < MAX_FIELD_NUMBER ? FIELD_ATOMS[code] : enif_make_int(env, code), value);
        
        // fprintf(stderr, "Read string %s -> '%.*s'\r\n", FIELD_NAMES[code], (int)(ptr - string_begin), string_begin);
        state = READING_CODE;
        code = 0;
        ptr++;
        continue;
      }
      
      case READING_BOOL: {
        ERL_NIF_TERM value;
        if(*ptr == 'Y' || *ptr == 'y') {
          value = atom_true;
        } else {
          value = atom_false;
        }
        ptr++;
        if(ptr == end || *ptr != 1) {
          //FIXME: clean reply
          return enif_make_tuple3(env, enif_make_atom(env, "error"), enif_make_atom(env, "reading_bool_soh"), enif_make_int(env, ptr - input.data));
        }
        
        reply[reply_size++] = enif_make_tuple2(env, code < MAX_FIELD_NUMBER ? FIELD_ATOMS[code] : enif_make_int(env, code), value);
        state = READING_CODE;
        code = 0;
        ptr++;
        continue;
      }
      default: {
        //FIXME: clean reply
        return enif_make_tuple3(env, enif_make_atom(env, "error"), enif_make_atom(env, "reading_fix"), enif_make_int(env, ptr - input.data));
        
      }
    }
  }
  
  ERL_NIF_TERM list = enif_make_list_from_array(env, reply, reply_size);
  free(reply);
  return list;
}

static ERL_NIF_TERM
field_by_number(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  char buf[100];
  int code = 0;
  if(enif_inspect_binary(env, argv[0], &bin)) {
    strncpy(buf, (char *)bin.data, bin.size);
    code = atoi(buf);
  } else if(enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1)) {
    code = atoi(buf);
  } else if(!enif_get_int(env, argv[0], &code)) {
    return enif_make_badarg(env);
  }
  
  return (code > 0 && code <= MAX_FIELD_NUMBER) ? FIELD_ATOMS[code] : enif_make_int(env, code);
}


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  int i;
  for(i = 0; i <= MAX_FIELD_NUMBER; i++) {
    FIELD_ATOMS[i] = enif_make_atom(env, FIELD_NAMES[i]);
  }
  
  for(i = 0; CHOICE_VALUES[i].desc; i++) {
    CHOICE_VALUES[i].atom = enif_make_atom(env, CHOICE_VALUES[i].desc);
  }
  
  atom_undefined = enif_make_atom(env, "undefined");
  atom_true = enif_make_atom(env, "true");
  atom_false = enif_make_atom(env, "false");
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
  {"split", 1, split},
  {"field_by_number", 1, field_by_number}
};


ERL_NIF_INIT(fix_splitter, fix_splitter_funcs, load, reload, upgrade, NULL)
