/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
png('t.png', width=10, height=3, unit='in', res=200, pointsize=9)
dx <- 10e3
dy <- 20e3
x <- seq(-200e3, 200e3, dx)
y <- seq(-200e3, 200e3, dy)
R <- 100e3
h <- outer(x, y, function(x, y) 500*exp(-(x^2+y^2)/R^2))
system("R CMD SHLIB gradient.c"); dyn.load('gradient.so'); grad<-.Call("gradient",h,x,y)
par(mfrow=c(1,3))
contour(x,y,h,asp=1)
f <- 1e-4
gprime <- 9.8 * 1 / 1024
u <- (gprime / f) * grad$gy
v <- -(gprime / f) * grad$gx
contour(x, y, u, asp=1)
contour(x, y, v, asp=1)
##print(grad$gx==100)
##print(grad$gy==100)

*/
#define ij(i, j) ((i) + (nrow) * (j))

SEXP gradient(SEXP m, SEXP x, SEXP y)
{
  int nrow = INTEGER(GET_DIM(m))[0];
  int ncol = INTEGER(GET_DIM(m))[1];
  if (nrow < 3)
    error("cannot handle case with nrow < 3 (FIXME)");
  if (ncol < 3)
    error("cannot handle case with ncol < 3 (FIXME)");
  if (LENGTH(x) != nrow)
    error("matrix has %d rows, but length(x) is %d", nrow, LENGTH(x));
  if (LENGTH(y) != ncol)
    error("matrix has %d cols, but length(y) is %d", ncol, LENGTH(y));
  SEXP lres;
  SEXP lres_names;
  PROTECT(lres = allocVector(VECSXP, 2));
  PROTECT(lres_names = allocVector(STRSXP, 2));
  double *mp = REAL(m);
  double *xp = REAL(x);
  double *yp = REAL(y);
  int i, j;

  // Calculate dm/dx, called gx here
  SEXP gx;
  PROTECT(gx = allocMatrix(REALSXP, nrow, ncol));
  double *gxp = REAL(gx);
  for (j = 0; j < ncol; j++) {
    gxp[ij(0, j)] = (mp[ij(1,j)] - mp[ij(0,j)]) / (xp[1] - xp[0]);
    gxp[ij(nrow - 1, j)] = (mp[ij(nrow - 1, j)] - mp[ij(nrow - 2, j)]) / (xp[nrow-1] - xp[nrow-2]);
  }
  for (i = 1; i < nrow - 1; i++) {
    for (j = 0; j < ncol; j++) {
      gxp[ij(i, j)] = (mp[ij(i+1, j)] - mp[ij(i-1,j)]) / (xp[i+1] - xp[i-1]);
    }
  }
  SET_VECTOR_ELT(lres, 0, gx);
  SET_STRING_ELT(lres_names, 0, mkChar("gx"));

  // Calculate dm/dy, called gy here
  SEXP gy;
  PROTECT(gy = allocMatrix(REALSXP, nrow, ncol));
  double *gyp = REAL(gy);
  for (i = 0; i < nrow; i++) {
    gyp[ij(i, 0)] = (mp[ij(i,1)] - mp[ij(i,0)]) / (yp[1] - yp[0]);
    gyp[ij(i, ncol - 1)] = (mp[ij(i, ncol - 1)] - mp[ij(i, ncol - 2)]) / (yp[ncol-1] - yp[ncol-2]);
  }
  for (i = 0; i < nrow; i++) {
    for (j = 1; j < ncol - 1; j++) {
      gyp[ij(i, j)] = (mp[ij(i, j+1)] - mp[ij(i,j-1)]) / (yp[j+1] - yp[j-1]);
    }
  }
  SET_VECTOR_ELT(lres, 1, gy);
  SET_STRING_ELT(lres_names, 1, mkChar("gy"));

  setAttrib(lres, R_NamesSymbol, lres_names);
  UNPROTECT(4);
  return(lres);
}

