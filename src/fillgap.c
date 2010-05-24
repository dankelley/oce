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
       x <- c(1:10, NA, NA, 13:15)
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
#ifdef DEBUG
// 
  for (i = 0; i < n; i++) {
    Rprintf("x[%d]=%f\n", i, xp[i]);
  }
#endif
  int last_ok, next_ok;
  double x_last_ok, x_next_ok;
  for (i = 0; i < xlen; i++) {
    if (ISNA(xp[i])) {
      last_ok = i - 1;          /* FIXME: what if NA at start? */
      if (i == 0)
        x_last_ok = 0.0;
      else
        x_last_ok = xp[last_ok];
      for (int j = i; j < xlen; j++) {
        if (!ISNA(xp[j])) {
          /* interpolate */
          for (int ij = last_ok + 1; ij < j; ij++) {
            resp[ij] = 999.99;       /* FIXME: interpolate here */
          }
          i = j - 1;
          break;
        }
      }
    } else {
      resp[i] = xp[i];
    }
  } /* FIXME: what if ends in NA? */
  UNPROTECT(2);
  return(res);
}
