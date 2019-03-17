#define RANDOM_CONSTANT 42

extern int global_var;

struct s_a {
  struct s_a* pointer;
  char array[16];
};

typedef struct s_b {
  struct {
    float enclosed_field;
  } record_field;
  double plain_field;
  struct s_a another_record_field;
} s_b;

enum e_a {
          c_0 = 10,
          c_1,
          c_2 = 20,
          c_3
};

union u_a {
  void* pointer;
  int value;
  enum e_a enumeration;
};

enum {
      anon_enum_0,
      anon_enum_1
};

struct s_d;
struct s_c {
  struct s_d forward;
};
struct s_d {
  struct s_c recursive_decl;
};

typedef long long plain_type_t;

int plain_function(int*, enum e_a);

void set_global_var(int);

int sbv_function(s_b arg);

union u_a sbv_return_function(struct s_a arg, char name[16]);
