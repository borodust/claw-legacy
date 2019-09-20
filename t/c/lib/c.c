#include "c.h"
#include "stdlib.h"
#include "string.h"

char tst_version_string_g[16] = "1.0.0";


char* tst_version_string() {
  return tst_version_string_g;
}

int count_nodes_and_assign_owner(tst_tree_t* owner, struct tst_node_t* node) {
  int node_count = 0;
  for (int i = 0; i < node->child_count; ++i) {
    node_count += count_nodes_and_assign_owner(owner, &node->children[i]);
  }
  node->owner = owner;
  return node_count + 1;
}

tst_tree_t* tst_create_tree(struct tst_node_t root) {
  tst_tree_t* tree = calloc(1, sizeof(tst_tree_t));
  tree->root = root;
  tree->info.node_count = count_nodes_and_assign_owner(tree, &tree->root);
  return tree;
}

struct tst_node_t tst_create_colored_node(union tst_color_t color) {
  struct tst_node_t node;
  node.children = NULL;
  node.child_count = 0;
  node.kind = tst_node_kind_colored;
  node.owner = NULL;

  struct tst_colored_node_t* data = calloc(1, sizeof(struct tst_colored_node_t));
  data->color = color;
  node.data = data;

  return node;
}

struct tst_node_t tst_create_named_node(char name[TST_NAME_MAX_LENGTH]) {
  struct tst_node_t node;
  node.children = NULL;
  node.child_count = 0;
  node.kind = tst_node_kind_named;
  node.owner = NULL;
  struct tst_named_node_t* data = calloc(1, sizeof(struct tst_named_node_t));
  strcpy(data->name, name);
  node.data = data;
  return node;
}

void tst_destroy_node(struct tst_node_t node) {
  free(node.data);
  free(node.children);
}

void tst_add_child(struct tst_node_t* parent, struct tst_node_t child) {
  struct tst_node_t* old_array = parent->children;
  parent->children = calloc(parent->child_count + 1, sizeof(struct tst_node_t));
  if (old_array != NULL) {
    memcpy(parent->children, old_array, parent->child_count * sizeof(struct tst_node_t));
    free(old_array);
  }
  parent->children[parent->child_count] = child;
  parent->children[parent->child_count].owner = parent->owner;
  parent->child_count += 1;
  if (parent->owner != NULL) {
    parent->owner->info.node_count += 1;
  }
}

enum tst_node_kind tst_get_node_kind(struct tst_node_t node) {
  return node.kind;
}

union tst_color_t tst_get_node_color(struct tst_node_t node) {
  return ((struct tst_colored_node_t*)node.data)->color;
}

void tst_set_node_color(struct tst_node_t* node, union tst_color_t color) {
  ((struct tst_colored_node_t*)node->data)->color = color;
}

char* tst_get_node_name(struct tst_node_t node) {
  return ((struct tst_named_node_t*)node.data)->name;
}

void tst_set_node_name(struct tst_node_t* node, char* name) {
  strcpy(((struct tst_named_node_t*)node->data)->name, name);
}

void tst_visit_nodes(struct tst_node_t* node, tst_node_visitor_t visitor) {
  visitor(node);
  for (int i = 0; i < node->child_count; ++i) {
    tst_visit_nodes(&node->children[i], visitor);
  }
}

void tst_visit_tree_nodes(tst_tree_t* tree, tst_node_visitor_t visitor) {
  tst_visit_nodes(&tree->root, visitor);
}

void tst_destroy_node_recursively(struct tst_node_t* node) {
  for(int i = 0; i < node->child_count; ++i) {
    tst_destroy_node_recursively(&node->children[i]);
  }
  tst_destroy_node(*node);
}

void tst_destroy_tree(tst_tree_t* tree) {
  tst_destroy_node_recursively(&tree->root);
}
