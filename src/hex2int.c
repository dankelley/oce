/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

// hex2int() takes a vector of character (hex) strings and returns a vector of
// numbers corresponding to their pairs, eg. "0110" yields c(1, 16).
// LIMITATION: all elements in the vector must have the same
//             number of characters, and this number must be even.

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
   TEST CODE

   system("R CMD shlib hex2int.c")
   dyn.load("hex2int.so")
   C <- c("0110", "00FF")
   I <- .Call("hex2int", C)
   I <- matrix(I, nrow=length(C), byrow=TRUE)

 */
int char2int(const char h)
{
    int I = 0;
    switch(h) {
        case '0': I = 0; break;
        case '1': I = 1; break;
        case '2': I = 2; break;
        case '3': I = 3; break;
        case '4': I = 4; break;
        case '5': I = 5; break;
        case '6': I = 6; break;
        case '7': I = 7; break;
        case '8': I = 8; break;
        case '9': I = 9; break;
        case 'A': I = 10; break;
        case 'B': I = 11; break;
        case 'C': I = 12; break;
        case 'D': I = 13; break;
        case 'E': I = 14; break;
        case 'F': I = 15; break;
        case 'a': I = 10; break;
        case 'b': I = 11; break;
        case 'c': I = 12; break;
        case 'd': I = 13; break;
        case 'e': I = 14; break;
        case 'f': I = 15; break;
    }
    return(I);
}

SEXP hex2int(SEXP C)
{
  PROTECT(C = AS_CHARACTER(C));
  int n = LENGTH(C);
  const char *Cp;
  SEXP res;

  Cp = CHAR(STRING_ELT(C, 0));
  int nchar = strlen(Cp);

  PROTECT(res = NEW_INTEGER((int)(n * nchar / 2)));
  int *resp = INTEGER_POINTER(res);
  int resi = 0;
  for (int i = 0; i < n; i++) {
      //Rprintf("i: %d\n", i);
      Cp = CHAR(STRING_ELT(C, i));
      //Rprintf("(%s) len %d\n", Cp, nchar);
      for (int c = 0; c < nchar; c+=2) {
          int I = 16 * char2int(Cp[c]) + char2int(Cp[c+1]);
          if (i < 2)
              Rprintf("  [%c %c | %d]\n", Cp[c], Cp[c+1], I); //int)Cp[c]);
          resp[resi++] = I;
      }
      //Rprintf("\n");
  }
  UNPROTECT(2);
  return(res);
}
