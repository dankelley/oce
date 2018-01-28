/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
system("R CMD SHLIB fillgap.c")
library(oce)
x <- c(1:2, NA, NA, 3:4)
dyn.load("fillgap.so")
xx <- .Call("fillgap", x, 1)
print(xx)
*/

/*#define DEBUG*/
SEXP fillgap1d(SEXP x, SEXP rule)
{
  int i;
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(rule = AS_INTEGER(rule));
  int the_rule = *INTEGER_POINTER(rule);
  //Rprintf("the_rule=%d\n", the_rule);
  double *xp = REAL(x);
  int xlen = LENGTH(x);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, xlen));
  unsigned char *isna = (unsigned char *) R_alloc(xlen, sizeof(unsigned char));
  double *resp = REAL(res);
  int last_ok;
  double x_last_ok = 0.0;
  for (i = 0; i < xlen; i++)
    isna[i] = (unsigned char)ISNA(xp[i]);
  for (i = 0; i < xlen; i++)
    resp[i] = xp[i];
  // End points: keep NA or set constant, according to 'rule' value.
  int first_good=-1, last_good=xlen; // set to values we can never get
  if (the_rule == 1) {
    ;
  } else if (the_rule == 2) {
    if (isna[0]) {
      for (i = 0; i < xlen; i++) {
        if (!isna[i]) {
          first_good = i;
          break;
        }
      }
      if (first_good == -1) {
        UNPROTECT(3);
        return(res); // whole vector is NA
      }
      for (i = 0; i < first_good; i++) {
        //Rprintf("setting resp[%d] with %f\n", i, resp[first_good]);
        resp[i] = resp[first_good];
      }
    }
    if (isna[xlen - 1]) {
      for (i = xlen - 1; i > -1; i--) {
        if (!isna[i]) {
          last_good = i;
          break;
        }
      }
      if (last_good == xlen) {
        UNPROTECT(3);
        return(res); // no good data (cannot happen; would be caught already)
      }
      for (i = xlen - 1; i > last_good; i--) {
        resp[i] = resp[last_good];
      }
    }
  } else {
    error("'rule' must be 1 or 2");
  }
  // Interior points: linear interpolation
  //Rprintf("first_good=%d last_good=%d\n", first_good, last_good);
  for (i = first_good + 1; i < last_good - 1; i++) {
    //Rprintf("main loop isna[%d] = %d\n", i, isna[i]);
    if (isna[i]) {
      last_ok = i - 1;
      x_last_ok = xp[last_ok];
      for (int j = i; j < xlen; j++) {
        if (!isna[j]) {
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
  UNPROTECT(3);
  return(res);
}
