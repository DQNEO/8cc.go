#include <stdio.h>

extern int f(int n);

int sum2(int a, int b) {
  return a + b;
}

int sum5(int a, int b, int c, int d, int e) {
  return a + b + c + d + e;
}

void xprintf(char *fmt, char *arg) {
 printf(fmt, arg);
}

int main(int argc, char **argv) {
  printf("%d\n", f(102));
  return 0;
}
