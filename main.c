#include <stdio.h>
#include <string.h>
#include "8cc.h"
#include <stdlib.h>
#define EXPR_LEN 100
extern char *block_to_string(Ast **block);

Ast **read_block(void) {
  Ast **block = malloc(sizeof(Ast) * EXPR_LEN);
  int i;
  for (i = 0; i < EXPR_LEN; i++) {
    Ast *t = read_decl_or_stmt();
    if (!t) break;
    block[i] = t;
  }
  block[i] = NULL;
  return block;
}


void emit_block(Ast **block) {
  int i;
  for(i = 0; block[i]; i++) {
    emit_expr(block[i]);
  }
}

int main(int argc, char **argv) {
  int wantast = (argc > 1 && !strcmp(argv[1], "-a"));

  Ast **block = read_block();
  if (wantast) {
    printf("%s", block_to_string(block));
  } else {
    print_asm_header();
    emit_block(block);
    printf("leave\n\t"
           "ret\n");
  }
  return 0;
}
