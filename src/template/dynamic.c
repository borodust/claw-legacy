#include <stddef.h>

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

{{function-pointers}}

__CLAW_API int {{loader-name}}(const char *name) {
  if(___claw_init_wrapper(name)) {

{{function-pointers-init}}

    ___claw_close_wrapper();
    return 0;
  }
  return 1;
}

{{function-definitions}}
