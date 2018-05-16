#include <stdio.h>
#include <string.h>
#include "8cc.h"

#include <stdlib.h>

Ast *ast_func(List *body) {
  Ast *r = malloc(sizeof(Ast));
  r->fname = "mymain";
  r->locals = locals;
  r->body = body;
  return r;
}

List *read_func_list() {
  List *block = read_block();
  Ast *r = ast_func(block);
  List *func_list = make_list();
  list_append(func_list, r);
  return func_list;
}
int main(int argc, char **argv) {
  int wantast = (argc > 1 && !strcmp(argv[1], "-a"));
  List *func_list = read_func_list();

  Iter *i = list_iter(func_list);
  Ast *f = iter_next(i);
  if (wantast) {
    printf("%s", block_to_string(f->body));
  } else {
    emit_data_section();
    emit_func(f);
  }
  return 0;
}
