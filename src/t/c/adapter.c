#include <stddef.h>
#include "c.h"


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

static int ___claw_init_wrapper(const char* module_name) {
  int wide_string_length = MultiByteToWideChar(CP_UTF8, 0, module_name, -1, NULL, 0);
  wchar_t* wide_module_name = calloc(wide_string_length, sizeof(wchar_t));
  MultiByteToWideChar(CP_UTF8, 0, module_name, -1, wide_module_name, wide_string_length);
  ___claw_module = LoadLibraryW(wide_module_name);
  free(wide_module_name);
  return ___claw_module != NULL;
}

static void ___claw_close_wrapper(void) {
  if(___claw_module != NULL) {
    FreeLibrary(___claw_module);
    ___claw_module = NULL;
  }
}
#else
#  include <dlfcn.h>
static void* ___claw_module;

static int ___claw_init_wrapper(const char* module_name) {
  ___claw_module = dlopen(module_name, RTLD_NOW | RTLD_GLOBAL);
  return ___claw_module != NULL;
}

static void ___claw_close_wrapper(void) {
  if(___claw_module != NULL) {
    dlclose(___claw_module);
    ___claw_module = NULL;
  }
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


static union u_a (*__v_claw_sbv_return_function)(sbv_return_function, sbv_return_function);
static int (*__v_claw_sbv_function)(sbv_function);

__CLAW_API int ___claw_libctest_loader_F6D054DFA7C9E57DFE1F11FC030D38CD79A3411A(const char *name) {
  if(___claw_init_wrapper(name)) {


__v_claw_sbv_return_function = claw_get_proc_addr("sbv_return_function");
__v_claw_sbv_function = claw_get_proc_addr("sbv_function");

    ___claw_close_wrapper();
    return 0;
  }
  return 1;
}

#if defined(__cplusplus)
extern "C" {
#endif


__CLAW_API union u_a* ___claw_sbv_return_function(union u_a* arg0, struct s_a* arg1, char[16] arg2) {
  union u_a result = __v_claw_sbv_return_function((*arg1), arg2);
 (*arg0) = result;
  return arg0;
}

__CLAW_API int ___claw_sbv_function(s_b* arg0) {
  int result = __v_claw_sbv_function((*arg0));
  return result;
}

#if defined(__cplusplus)
}
#endif
