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
int main(int argc, char **argv) {
  int wantast = (argc > 1 && !strcmp(argv[1], "-a"));
  List *block = read_block();

  Ast *r = ast_func(block);
  if (wantast) {
    printf("%s", block_to_string(r->body));
  } else {
    emit_data_section();
    emit_func(r);
  }
  return 0;
}
