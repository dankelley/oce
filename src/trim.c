/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
// Find start and stop indices in x that enclose xlim with
// one extra element less than xlim[1] and one more than xlim[2].

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
 
   library(oce)
   system("R CMD SHLIB trim.c")
   dyn.load("trim.so")
   x<-seq(0, 10, 0.1)
   xlim<-c(2.0, 2.9)
   a <- .Call("trim_ts", x, xlim)
   str(a)
   xlim
   range(x)
   x[a$from:a$to]

*/

/*#define DEBUG*/
SEXP trim_ts(SEXP x, SEXP xlim, SEXP extra)
{
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(xlim = AS_NUMERIC(xlim));
  PROTECT(extra = AS_NUMERIC(extra));
  double *xp = REAL(x);
  double *xlimp = REAL(xlim);
  double *extrap = REAL(extra);
  int nx= LENGTH(x);
  int nxlim = LENGTH(xlim);
  if (nxlim != 2)
    error("In trim_ts(), length of xlim must be 2");
  if (xlimp[1] < xlimp[0])
    error("In trim_ts(), xlim must be ordered but it is (%g, %g)\n", xlimp[0], xlimp[1]);
  for (int i = 1; i < nx; i++) {
    if (xp[i] == xp[i-1]) {
      error("In trim_ts(), x must be distinct but x[%d]=x[%d]=%.10g\n", i-1, i, xp[i-1]);
    } else if (xp[i] < xp[i-1]) {
      error("In trim_ts(), x must be ordered but x[%d]=%.10g and x[%d]=%.10g\n", i-1, xp[i-1], i, xp[i]);
    }
  }
  double epsilon = (xp[1] - xp[0]) / 1e9;

  SEXP from;
  SEXP to;
  PROTECT(from = NEW_NUMERIC(1));
  PROTECT(to = NEW_NUMERIC(1));

  double start = xlimp[0] - (*extrap)*(xlimp[1]-xlimp[0]) - epsilon;
  double end = xlimp[1] + (*extrap)*(xlimp[1]-xlimp[0]) + epsilon;

  double *fromp = REAL(from);
  double *top = REAL(to);
  for (int i = 0; i < nx; i++) {
    //Rprintf("examine x[%d]=%f\n", 1+i, xp[i]);
    if (xp[i] >= start) {
      *fromp = (double)i;//-1;
      break;
    }
  }
  for (int i = nx-1; i >= 0; i--) {
    //Rprintf("examine x[%d]=%f\n", 1+i, xp[i]);
    if (xp[i] < end) {
      *top = (double)i+2;
      break;
    }
  }
  if (*fromp < 1.0) *fromp = 1.0;
  if (*top > nx) *top = (double)nx;

  SEXP res, res_names;
  PROTECT(res = allocVector(VECSXP, 2));
  PROTECT(res_names = allocVector(STRSXP, 2));
  SET_VECTOR_ELT(res, 0, from);
  SET_STRING_ELT(res_names, 0, mkChar("from"));
  SET_VECTOR_ELT(res, 1, to);
  SET_STRING_ELT(res_names, 1, mkChar("to"));
  setAttrib(res, R_NamesSymbol, res_names);

  UNPROTECT(7);
  return(res);
}

