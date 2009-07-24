/*#include <stdio.h>		/* debug */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
 * compile from commandline:
 R CMD SHLIB bitwise.c
 * test R code:
 buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
 dyn.load("bitwise.so")
 m <- .Call("match2bytes", buf, as.raw(0xa5), as.raw(0x11))
 print(m)
*/

SEXP match2bytes(SEXP buf, SEXP m1, SEXP m2)
{
  int i, j, n, n_match;
  double *resp;
  unsigned char *bufp, *m1p, *m2p;
  SEXP res;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(m1 = AS_RAW(m1));
  PROTECT(m2 = AS_RAW(m2));
  bufp = RAW_POINTER(buf);
  m1p = RAW_POINTER(m1);
  m2p = RAW_POINTER(m2);
  n = LENGTH(buf);
  n_match = 0;
  for (i = 0; i < n - 1; i++) {
    if (bufp[i] == *m1p && bufp[i + 1] == *m2p) {
      n_match++;
      ++i;			/* skip */
    }
  }
  PROTECT(res = NEW_NUMERIC(n_match));
  resp = NUMERIC_POINTER(res);
  j = 0;
  for (i = 0; i < n - 1; i++) {
    if (j <= n_match && bufp[i] == *m1p && bufp[i + 1] == *m2p) {
      resp[j++] = i + 1;	/* the 1 is to offset from C to R */
    }
  }
  UNPROTECT(4);
  return(res);
}

SEXP match3bytes(SEXP buf, SEXP m1, SEXP m2, SEXP m3)
{
  int i, j, n, n_match;
  double *resp;
  unsigned char *bufp, *m1p, *m2p, *m3p;
  SEXP res;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(m1 = AS_RAW(m1));
  PROTECT(m2 = AS_RAW(m2));
  PROTECT(m3 = AS_RAW(m3));
  bufp = RAW_POINTER(buf);
  m1p = RAW_POINTER(m1);
  m2p = RAW_POINTER(m2);
  m3p = RAW_POINTER(m3);
  n = LENGTH(buf);
  n_match = 0;
  for (i = 0; i < n - 2; i++) {
    if (bufp[i] == *m1p && bufp[i + 1] == *m2p && bufp[i + 2] == *m3p) {
      n_match++;
      ++i;			/* skip */
      ++i;			/* skip */
    }
  }
  PROTECT(res = NEW_NUMERIC(n_match));
  resp = NUMERIC_POINTER(res);
  j = 0;
  for (i = 0; i < n - 2; i++) {
    if (j <= n_match && bufp[i] == *m1p && bufp[i + 1] == *m2p && bufp[i + 2] == *m3p) {
      resp[j++] = i + 1;	/* the 1 is to offset from C to R */
    }
  }
  UNPROTECT(5);
  return(res);
}


SEXP oce_filter(SEXP b, SEXP a, SEXP x)
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
    int ioffset;		/* prevent looking before start */
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
