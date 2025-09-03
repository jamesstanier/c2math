// Minimal source to drive IR → JSON → C for FR-009 features.

enum E { A = 1, B = 2, C = 5 };

struct S {
  int a;
  unsigned int b : 3;
  char name[16];
  int *p;
};

typedef int (*cmp_t)(const void *lhs, const void *rhs);

static const int *gp;
char buf[16];

int sort(cmp_t cmp, void *arr, int n);
float *mk(void);
int log_msg(const char *fmt, ...);
