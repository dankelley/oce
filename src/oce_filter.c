/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

SEXP oce_filter(SEXP x, SEXP a, SEXP b)
{
  int i, ib, ia, nb, na, nx;
  double *bp, *ap, *xp, *yp;
  SEXP y;
  PROTECT(b = AS_NUMERIC(b));
  PROTECT(a = AS_NUMERIC(a));
  PROTECT(x = AS_NUMERIC(x));
  bp = NUMERIC_POINTER(b);
  ap = NUMERIC_POINTER(a);
  xp = NUMERIC_POINTER(x);
  nb = LENGTH(b);
  na = LENGTH(a);
  nx = LENGTH(x);
  PROTECT(y = NEW_NUMERIC(nx));
  yp = NUMERIC_POINTER(y);
  for (i = 0; i < nx; i++) {
    double xsum, ysum;
    int ioffset; /* prevent looking before start */
    xsum = 0.0;
    for (ib = 0; ib < nb; ib++) {
      ioffset = i - ib;
      if (ioffset > -1)
          xsum += bp[ib] * xp[ioffset];
    }
    ysum = 0.0;
    for (ia = 1; ia < na; ia++) {
      ioffset = i - ia;
      if (ioffset > -1)
          ysum += ap[ia] * yp[ioffset];
    }
    yp[i] = xsum - ysum;
  }
  UNPROTECT(4);
  return(y);
}
