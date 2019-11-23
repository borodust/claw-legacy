/*
 * THIS API IS SPECIFICALLY MADE TO TEST VARIOUS ASPECTS OF SPEC GENERATION
 * THIS IS NOT HOW YOU SHOULD DESIGN ANY SANE C++ API
 */
#include "stdint.h"

#define TST_VERSION 1
#define TST_NAME_MAX_LENGTH 16

extern char tst_version_string_g[16];

namespace test {
  enum {
        tst_black = 0x0,
        tst_white = 0xFFFFFFFF
  };
}
