/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/* 

   x <- 1:100
   y <- 1 + 2 * x + x^2/50 + 200*sin(x/5)
   L <- 5
   system("R CMD SHLIB run.c")
   dyn.load('run.so')
   calc <- .Call("run_lm", x, y, L, 0)
   theory <- 2 + 0.04 * x + 40*cos(x/5)
   compare <- data.frame(calc=calc, theory=theory)
   par(mfrow=c(2,1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
   plot(x, y, type='l')
   lines(x[1]+c(0, L), rep(mean(y), 2), col='blue', lwd=3)
   plot(x, theory, type='l', ylab="dy/dx")
   lines(x[1]+c(0, L), rep(mean(theory), 2), col='blue', lwd=3)
   lines(x, calc, col='red')
   legend("top", lwd=1, col=c("black","red"),
           legend=c("theory", "calc"))


*/

SEXP run_lm(SEXP x, SEXP y, SEXP xout, SEXP window, SEXP L, SEXP deriv)
{
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(xout = AS_NUMERIC(xout));
  PROTECT(L = AS_NUMERIC(L));
  PROTECT(window = AS_NUMERIC(window));
  PROTECT(deriv = AS_NUMERIC(deriv));
  int nx = LENGTH(x);
  int ny = LENGTH(y);
  if (nx != ny)
    error("length(x) is %d but length(y) is %d\n", nx, ny);
  int nxout = LENGTH(xout);
  double *xp = REAL(x);
  double *yp = REAL(y);
  double *xoutp = REAL(xout);
  int nL = LENGTH(L);
  double *Lp = REAL(L);
  double L2 = Lp[0] / 2;
  SEXP res;
  PROTECT(res = allocVector(REALSXP, nxout));
  double *resp = REAL(res);
  double *windowp = REAL(window);
  int windowType = (int)floor(0.5 + *windowp);
  double *derivp = REAL(deriv);
  int derivType = (int)floor(0.5 + * derivp);
  if (windowType == 0) {
    for (int i = 0; i < nxout; i++) {
      double Sx = 0.0, Sy = 0.0, Sxx = 0.0, Sxy = 0.0;
      int n = 0;
      for (int j=0; j < nx; j++) {
	double w, d;
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
	if (derivType == 0) {
	  resp[i] = A + B * xoutp[i];
	} else {
	  resp[i] = (n * Sxy - Sx * Sy) / (n * Sxx - Sx * Sx);
	}
      } else {
	resp[i] = NA_REAL;
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
	if (derivType == 0) {
	  resp[i] = A + B * xoutp[i];
	} else {
	  resp[i] = B;
	}
      } else {
	resp[i] = NA_REAL;
      }
    } // i
  } else {
    error("invalid window type (internal coding error in run.c)\n");
  }
  UNPROTECT(7);
  return(res);
}

