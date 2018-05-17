#include <stdio.h>

extern int f(int n);

void xprintf(char *fmt, char *arg) {
 printf(fmt, arg);
}

int main(int argc, char **argv) {
  printf("%d\n", f(102));
  return 0;
}
