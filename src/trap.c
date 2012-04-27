#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
   system("R CMD SHLIB trap.c")
   x <- seq(0, 1, 0.1)
   y <- 2 * x
   dyn.load("trap.so")
   print(.Call("trap", x, y, FALSE))
   print(.Call("trap", x, y, TRUE))
   */

SEXP trap(SEXP x, SEXP y, SEXP cumulative)
{
  int i;
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(cumulative = AS_INTEGER(cumulative));
  int nx = length(x), ny = length(y);
  if (nx != ny)
      error("lengths of x (%d) and y (%d) must match", nx, ny);
  double *xp = REAL(x), *yp = REAL(y);
  int *cumulativep = INTEGER(cumulative);
  SEXP res;
  if (*cumulativep) {
      PROTECT(res = NEW_NUMERIC(nx));
      double *resp = REAL(res);
      resp[0] = 0.0;
      for (int i = 1; i < nx; i++) {
          //Rprintf("%d %f %f\n", i, xp[i], yp[i]);
          resp[i] = 0.5 * (yp[i] + yp[i-1]) * (xp[i] - xp[i-1]);
      }
  } else {
      PROTECT(res = NEW_NUMERIC(1));
      double *resp = REAL(res);
      *resp = 0.0;
      for (int i = 1; i < nx; i++) {
          *resp += 0.5 * (yp[i] + yp[i-1]) * (xp[i] - xp[i-1]);
      }
  }
  UNPROTECT(4);
  return(res);
}


