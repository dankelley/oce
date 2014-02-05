/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/* 

   x <- 1:50
   y <- 2 + 3 * x + 4 * x^2 + 5 * x^3
   # actual ans: 3 + 8 * x
   w <- 4
   system("R CMD SHLIB running_derivative.c")
   dyn.load('running_derivative.so')
   calc <- .Call("running_derivative", x, y, w)
   theory <- 3 + 8 * x + 15 * x^2
   compare <- data.frame(calc=calc, theory=theory)
   plot(x, theory, type='o', ylab="dy/dx", pch=20)
   points(x, calc, col='red')
   legend("topleft", pch=c(20, 1), col=c("black","red"),
           legend=c("theory", "calc"))


*/
#define ix(i, j) ((i) + (nrow) * (j))

SEXP running_derivative(SEXP x, SEXP y, SEXP w)
{
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(w = AS_NUMERIC(w));
  int nx = LENGTH(x);
  int ny = LENGTH(y);
  if (nx != ny)
    error("length(x) is %d but length(y) is %d\n", nx, ny);
  double *xp = REAL(x);
  double *yp = REAL(y);
  int nw = LENGTH(w);
  double *wp = REAL(w);
#ifdef DEBUG
  Rprintf("nx: %d, ny: %d, nw: %d\n", nx, ny, nw);
#endif
  SEXP res;
  PROTECT(res = allocVector(REALSXP, nx));
  double *resp = REAL(res);
#ifdef DEBUG
  Rprintf("w: %f\n", *wp, "\n");
#endif
  if (1 == length(w)) {
    // formula: sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
    for (int i = 0; i < nx; i++) {
      double xsum = 0.0, ysum = 0.0;
      int n = 0;
      for (int j=0; j < nx; j++) {
	if (abs(xp[i] - xp[j]) < *wp) {
	  xsum += xp[j];
	  ysum += yp[j];
	  n++;
	}
      }
      double xbar = xsum / n;
      double ybar = ysum / n;
#ifdef DEBUG
      Rprintf("xp[%d]: %f, xbar: %f, ybar: %f\n", i, xp[i], xbar, ybar);
#endif
      double Sxy = 0, Sxx = 0.0;
      for (int j=0; j < nx; j++) {
	if (abs(xp[i] - xp[j]) < *wp) {
	  Sxy += (xp[j] - xbar) * (yp[j] - ybar);
	  Sxx += (xp[j] - xbar) * (xp[j] - xbar);
	}
      }
      resp[i] = Sxy / Sxx;
    }
  } else {
    error("at least for now, length(w) must be 1\n");
  }
  UNPROTECT(4);
  return(res);
}

