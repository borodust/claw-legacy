/*
 * THIS API IS SPECIFICALLY MADE TO TEST VARIOUS ASPECTS OF SPEC GENERATION
 * THIS IS NOT HOW YOU SHOULD DESIGN ANY SANE C API
 */
#include "stdint.h"

#define TST_VERSION 1
#define TST_NAME_MAX_LENGTH 16

extern char tst_version_string_g[16];

enum {
  tst_black = 0x0,
  tst_white = 0xFFFFFFFF
};


typedef struct {
  void* __data;
} __metadata_t;


typedef struct {
  __metadata_t data;
} metadata_t;


union tst_color_t {
  uint32_t encoded;
  struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
  } component;
  uint8_t array[4];
};

enum tst_node_kind {
  tst_node_kind_unknown = 10,
  tst_node_kind_named,
  tst_node_kind_colored = 20
};


struct tst_named_node_t {
  char name[TST_NAME_MAX_LENGTH + 1];
};

struct tst_colored_node_t {
  union tst_color_t color;
};

struct tst_tree_t;
struct tst_node_t {
  struct tst_node_t* children;
  int child_count;
  enum tst_node_kind kind;
  struct tst_tree_t* owner;
  void* data;
};

typedef void (*tst_node_visitor_t)(struct tst_node_t*);

typedef struct tst_tree_t {
  struct tst_node_t root;
  struct {
    int node_count;
  } info;
  metadata_t _meta;
} tst_tree_t;


char* tst_version_string();

tst_tree_t* tst_create_tree(struct tst_node_t root);

void tst_destroy_tree(tst_tree_t* tree);

struct tst_node_t tst_create_colored_node(union tst_color_t);

struct tst_node_t tst_create_named_node(char* name);

void tst_destroy_node(struct tst_node_t node);

void tst_add_child(struct tst_node_t* parent, struct tst_node_t child);

enum tst_node_kind tst_get_node_kind(struct tst_node_t node);

union tst_color_t tst_get_node_color(struct tst_node_t node);

void tst_set_node_color(struct tst_node_t* node, union tst_color_t);

char* tst_get_node_name(struct tst_node_t node);

void tst_set_node_name(struct tst_node_t* node, char*);

void tst_visit_tree_nodes(tst_tree_t* tree, tst_node_visitor_t visitor);
