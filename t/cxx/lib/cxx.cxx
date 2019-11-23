#include "cxx.hxx"
#include "stdlib.h"
#include "string.h"

char tst_version_string_g[16] = "1.0.0";


char* tst_version_string() {
  return tst_version_string_g;
}
