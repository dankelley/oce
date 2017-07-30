/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/* 

   x <- 1:20
   y <- 1 + 2 * x + x^2/50 + 200*sin(x/5)
   L <- 10
   system("R CMD SHLIB run.c")
   dyn.load('run.so')
   source("../R/run.R")
   calc <- runlm(x, y, L=L)
   plot(x,y,type='l'); lines(x, calc$y, col='red')

*/

SEXP run_lm(SEXP x, SEXP y, SEXP xout, SEXP window, SEXP L)
{
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(xout = AS_NUMERIC(xout));
  PROTECT(L = AS_NUMERIC(L));
  PROTECT(window = AS_NUMERIC(window));
  int nx = LENGTH(x);
  int ny = LENGTH(y);
  if (nx != ny)
    error("length(x) is %d but length(y) is %d\n", nx, ny);
  int nxout = LENGTH(xout);
  double *xp = REAL(x);
  double *yp = REAL(y);
  double *xoutp = REAL(xout);
  double *Lp = REAL(L);
  double L2 = Lp[0] / 2;
  double *windowp = REAL(window);
  int windowType = (int)floor(0.5 + *windowp);

  SEXP res, res_names, Y, dYdx, Lout;
  PROTECT(res = allocVector(VECSXP, 4));
  PROTECT(res_names = allocVector(STRSXP, 4));
  // xout
  PROTECT(Y = allocVector(REALSXP, nxout));
  PROTECT(dYdx = allocVector(REALSXP, nxout));
  PROTECT(Lout = allocVector(REALSXP, 1));
  double *Yp = REAL(Y);
  double *dYdxp = REAL(dYdx);

  if (windowType == 0) {
    for (int i = 0; i < nxout; i++) {
      double Sx = 0.0, Sy = 0.0, Sxx = 0.0, Sxy = 0.0;
      int n = 0;
      for (int j=0; j < nx; j++) {
        double l = fabs(xoutp[i] - xp[j]);
        if (l < L2) {
          Sx += xp[j];
          Sy += yp[j];
          Sxx += xp[j] * xp[j];
          Sxy += xp[j] * yp[j];
          n++;
        }
      } // j
      if (n > 1) {
        double A = (Sxx * Sy - Sx * Sxy) / (n * Sxx - Sx * Sx);
        double B = (n * Sxy - Sx * Sy) / (n * Sxx - Sx * Sx);
        Yp[i] = A + B * xoutp[i];
        dYdxp[i] = B;
      } else {
        Yp[i] = NA_REAL;
        dYdxp[i] = NA_REAL;
      }
    } // i
  } else if (windowType == 1) {
    double pi = 3.141592653589793116;
    for (int i = 0; i < nxout; i++) {
      double Swwx = 0.0, Swwy = 0.0, Swwxx = 0.0, Swwxy = 0.0, Sww = 0.0;
      int n = 0;
      for (int j=0; j < nx; j++) {
        double l = fabs(xoutp[i] - xp[j]);
        if (l < L2) {
          double ww = 0.5 * (1 + cos(pi * l / L2));
          Swwx += ww * xp[j];
          Swwy += ww * yp[j];
          Swwxx += ww * xp[j] * xp[j];
          Swwxy += ww * xp[j] * yp[j];
          Sww += ww;
          n++;
        }
      } // j
      if (n > 1) {
        double A = (Swwxx * Swwy - Swwx * Swwxy) / (Sww * Swwxx - Swwx * Swwx);
        double B = (Sww * Swwxy - Swwx * Swwy) / (Sww * Swwxx - Swwx * Swwx);
        Yp[i] = A + B * xoutp[i];
        dYdxp[i] = B;
      } else {
        Yp[i] = NA_REAL;
        dYdxp[i] = NA_REAL;
      }
    } // i
  } else {
    error("invalid window type (internal coding error in run.c)\n");
  }
  SET_VECTOR_ELT(res, 0, xout);
  SET_STRING_ELT(res_names, 0, mkChar("x"));
  SET_VECTOR_ELT(res, 1, Y);
  SET_STRING_ELT(res_names, 1, mkChar("y"));
  SET_VECTOR_ELT(res, 2, dYdx);
  SET_STRING_ELT(res_names, 2, mkChar("dydx"));
  SET_VECTOR_ELT(res, 3, L);
  SET_STRING_ELT(res_names, 3, mkChar("L"));
  setAttrib(res, R_NamesSymbol, res_names);
  UNPROTECT(10);
  return(res);
}

