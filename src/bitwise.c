#include <R.h>
#include <Rdefines.h>

void
match2bytes(int *n,  unsigned char *input, unsigned char *m1, unsigned char *m2, unsigned char *match)
{
  int i;
  for (i = 0; i < *n - 1; i++) {
    if (input[i] == *m1 && input[i+1] == *m2) {
      match[i] = 0x01;
      match[++i] = 0x00;
    } else {
      match[i] = 0x00;
    }
  }
}

void
match3bytes(int *n,  unsigned char *input, unsigned char *m1, unsigned char *m2, unsigned char *m3, unsigned char *match)
{
  int i;
  for (i = 0; i < *n - 2; i++) {
    if (input[i] == *m1 && input[i+1] == *m2 && input[i+2] == *m3) {
      match[i] = 0x01;
      match[++i] = 0x00;
      match[++i] = 0x00;
    } else {
      match[i] = 0x00;
    }
  }
}
