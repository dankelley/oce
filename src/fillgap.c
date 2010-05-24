#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
 1. compile from commandline:
    ::

      R CMD SHLIB fillgap.c
   
 2. test R code:
     ::

       library(oce)
       x <- c(1:2, NA, NA, 3:4)
       dyn.load("fillgap.so"); xx <- .Call("fillgap", x); print(xx)

*/

/*#define DEBUG*/
SEXP fillgap(SEXP x)
{
  int i;
  PROTECT(x = AS_NUMERIC(x));
  double *xp = REAL(x);
  int xlen = LENGTH(x);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, xlen));
  double *resp = REAL(res);
  int last_ok, next_ok;
  double x_last_ok = 0.0;
  if (ISNA(xp[0]) || ISNA(xp[xlen - 1])) {
    for (i = 0; i < xlen; i++)
      resp[i] = xp[i];
  } else {
    for (i = 0; i < xlen; i++) {
      if (ISNA(xp[i])) {
        last_ok = i - 1;
        x_last_ok = xp[last_ok];
        for (int j = i; j < xlen; j++) {
          if (!ISNA(xp[j])) {
            for (int ij = last_ok + 1; ij < j; ij++) {
              resp[ij] = x_last_ok + (ij - last_ok) * (xp[j] - x_last_ok) / (j - i + 1);
            }
            i = j - 1;
            break;
          }
        }
      } else {
        resp[i] = xp[i];
      }
    }
  }
  UNPROTECT(2);
  return(res);
}
