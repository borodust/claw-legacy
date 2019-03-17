#include "c.h"

int global_var = RANDOM_CONSTANT;


void set_global_var(int new_value) {
  global_var = new_value;
}


int plain_function(int* arg, enum e_a enumeration) {
  if (enumeration == c_0) {
    *arg = global_var;
  } else {
    *arg = enumeration;
  }
  return *arg;
}


int sbv_function(s_b arg) {
    return arg.plain_field;
}


union u_a sbv_return_function(struct s_a arg, char name[16]) {
  union u_a result;
  result.pointer = arg.pointer;
  return result;
}
