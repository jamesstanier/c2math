//#include <stdio.h>

typedef int myint;
struct S { int x; };
enum E { A = 1, B };

static int g;

int add(myint a, int b) {
  struct S s = { .x = a };
  int y = b;
  return s.x + y + g;
}
