/* Generated by :claw at {{timestamp}} */

#include <stddef.h>
{{includes}}


#if !defined(__CLAW_API)
#  if defined(_WIN32)
#    define __CLAW_API __declspec(dllexport)
#  elif defined(__GNUC__)
#    define __CLAW_API __attribute__((visibility("default")))
#  else
#    define __CLAW_API
#  endif
#endif

#if defined(__cplusplus)
extern C {
#endif

#ifdef _WIN32
#  include <windows.h>
static HMODULE ___claw_module;

static int ___claw_init_wrapper() {
  ___claw_module = GetModuleHandle(NULL);
  return ___claw_module != NULL;
}

static void ___claw_close_wrapper(void) {
}
#else
#  include <dlfcn.h>
static void* ___claw_module;

static int ___claw_init_wrapper() {
  ___claw_module = dlopen(NULL, RTLD_NOW | RTLD_GLOBAL);
  return ___claw_module != NULL;
}

static void ___claw_close_wrapper(void) {
}
#endif

static void* claw_get_proc_addr(const char *name) {
  if(___claw_module == NULL) {
    return NULL;
  }

#ifdef _WIN32
  return (void*) GetProcAddress(___claw_module, name);
#else
  return dlsym(___claw_module, name);
#endif
}

#if defined(__cplusplus)
extern "C" {
#endif

{{function-types}}

{{function-pointers}}

__CLAW_API int {{loader-name}}() {
  if(___claw_init_wrapper()) {

{{function-pointers-init}}

    ___claw_close_wrapper();
    return 0;
  }
  return 1;
}

{{function-definitions}}

#if defined(__cplusplus)
}
#endif
