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
  for (i = 0; i < xlen; i++) {
    resp[i] = xp[i];
  }
  UNPROTECT(2);
  return(res);
}
