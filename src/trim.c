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
SEXP trim_ts(SEXP x, SEXP xlim)
{
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(xlim = AS_NUMERIC(xlim));
  double *xp = REAL(x);
  double *xlimp = REAL(xlim);
  int nx= LENGTH(x);
  int nxlim = LENGTH(xlim);
  if (nxlim != 2) error("length of xlim must be 2");
  if (xlimp[1] < xlimp[0]) error("xlim must be ordered");
  for (int i = 1; i < nx; i++) if (xp[i] <= xp[i-1]) error("x must be ordered");
  double epsilon = (xp[1] - xp[0]) / 1e3;

  SEXP from;
  SEXP to;
  PROTECT(from = NEW_NUMERIC(1));
  PROTECT(to = NEW_NUMERIC(1));

  double *fromp = REAL(from);
  double *top = REAL(to);
  int ifrom = nx, ito=-1;
  for (int i = 0; i < nx; i++) {
    //Rprintf("examine x[%d]=%f\n", 1+i, xp[i]);
    if (xp[i] > (xlimp[0] - epsilon)) {
      *fromp = (double)i;//-1;
      break;
    }
  }
  for (int i = nx-1; i >= 0; i--) {
    //Rprintf("examine x[%d]=%f\n", 1+i, xp[i]);
    if (xp[i] < (xlimp[1] + epsilon)) {
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

  UNPROTECT(6);
  return(res);
}

