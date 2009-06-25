#include <R.h>
#include <Rdefines.h>

void
match2bytes(int *n,  unsigned char *input, unsigned char *m1, unsigned char *m2, int *match)
{
  int i;
  for (i = 0; i < *n - 1; i++) {
    if (input[i] == *m1 && input[i+1] == *m2) {
      match[i] = 1;
      match[++i] = 0;
    } else {
      match[i] = 0;
    }
  }
}
