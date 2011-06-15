/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>

//#define DEBUG

/*
To build this, build Oce or just do as follows:

system("R CMD SHLIB bisect.c"); dyn.load("bisect.so"); .Call("bisect", seq(0, 10, 0.1), 2.1)

To use this, do the following ONE TIME in a given program:

dyn.load("bisect.so")

To use it, do e.g.

.Call("bisect", seq(0, 10, 0.1), 2.1)

which searches for 2.1 in the list (0, 0.1, 0.2,...).

*/

SEXP bisect(SEXP x, SEXP X)
{
  // SYNOPSIS: bisection search
  // REQUIREMENT: x must be in order from smaller to larger
  // x = vector of numerical values
  // X = value to search for
  //
  // RETURN VALUE: The index (in R notation, starting at 1) of first item for which
  // x <= X & X < x
  //
  PROTECT(x= AS_NUMERIC(x));
  PROTECT(X= AS_NUMERIC(X));
  double *xp = REAL(x);
  double *Xp = REAL(X);

  unsigned int n = LENGTH(x);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, 1));
  double *resp = REAL(res);

  if (n < 1) {
    resp[0] = 0;
  } else {
    if (Xp[0] <= xp[0]) {
      resp[0] = 1;
    } else if (xp[n-1] <= Xp[0]) {
      resp[0] = n;
    } else {
      unsigned int lower = 0, upper = n-1, middle;
      int npass = floor(5.0 + log2(0.0 + n)); // for safety, add 5; should never get close
      for (unsigned int pass = 0; pass < npass; pass++) {
	middle = (int)floor((upper + lower) / 2.0);
	if (Xp[0] <= xp[middle])
	  upper = middle;
	else
	  lower = middle;
	if ((upper - lower) < 2)
	  break;
#ifdef DEBUG
	Rprintf("pass %d   lower %d   upper %d   middle %d\n", pass, lower, upper, middle);
#endif
      }	
      resp[0] = middle + 1.0;
    }
  }
  UNPROTECT(3);
  return(res);
}
