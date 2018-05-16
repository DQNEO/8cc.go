#include <stdio.h>
#include <string.h>
#include "8cc.h"


int main(int argc, char **argv) {
  int wantast = (argc > 1 && !strcmp(argv[1], "-a"));
  List *func_list = read_func_list();

  if (!wantast)
    emit_data_section();

  for (Iter *i = list_iter(func_list); !iter_end(i);) {
    Ast *func = iter_next(i);
    if (wantast)
      printf("%s", block_to_string(func->body));
    else
      emit_func(func);
  }
  return 0;
}
