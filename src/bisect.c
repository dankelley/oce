/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>

//#define DEBUG

/*
   To build this, build Oce or just do as follows:

   system("R CMD SHLIB bisect.c")
   dyn.load("bisect.so")

   To use it, do e.g.

   x <- seq(0,10,0.1)
   i <- .Call("bisect", x, c(2.1,3,3.3))
   print(x[i])

   which searches for 2.1, 3 and 3.3 in the list 0, 0.1, 0.2, ... ,10.

*/

SEXP bisect(SEXP x, SEXP X)
{
  // SYNOPSIS: bisection search
  // REQUIREMENT: x must be in order from smaller to larger
  // x = vector of numerical values
  // X = value(s) to search for
  //
  // RETURN VALUE: The index(es) (in R notation, starting at 1) of first item for which
  //    x <= X & X < x
  // for each X.
  //
  PROTECT(x= AS_NUMERIC(x));
  PROTECT(X= AS_NUMERIC(X));
  double *xp = REAL(x);
  double *Xp = REAL(X);

  unsigned int nx = LENGTH(x);
  unsigned int nX = LENGTH(X);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, nX));
  double *resp = REAL(res);

  for (int iX = 0; iX < nX; iX++) {

    if (nx < 1) {
      resp[iX] = 0;
    } else {
      if (Xp[iX] <= xp[0]) {
        resp[iX] = 1;
      } else if (xp[nx-1] <= Xp[iX]) {
        resp[iX] = nx;
      } else {
        unsigned int lower = 0, upper = nx-1, middle = nx / 2;
        int npass = floor(5.0 + log2(0.0 + nx)); // for safety, add 5; should never get close
        for (unsigned int pass = 0; pass < npass; pass++) {
          middle = (int)floor((upper + lower) / 2.0);
          if (Xp[iX] <= xp[middle])
            upper = middle;
          else
            lower = middle;
          if ((upper - lower) < 2)
            break;
#ifdef DEBUG
          Rprintf("pass %d   lower %d   upper %d   middle %d\n", pass, lower, upper, middle);
#endif
        }
        resp[iX] = middle + 1.0;
      }
    }
  }
  UNPROTECT(3);
  return(res);
}
