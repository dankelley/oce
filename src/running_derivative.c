/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/* 

   x <- 1:100
   y <- 1 + 2 * x + x^2/50 + 200*sin(x/5)
   xrange <- 4
   system("R CMD SHLIB running_derivative.c")
   dyn.load('running_derivative.so')
   calc <- .Call("running_derivative", x, y, xrange)
   theory <- 2 + 0.04 * x + 40*cos(x/5)
   compare <- data.frame(calc=calc, theory=theory)
   par(mfrow=c(2,1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
   plot(x, y, type='l')
   lines(x[1]+c(0, xrange), rep(mean(y), 2), col='blue', lwd=3)
   plot(x, theory, type='l', ylab="dy/dx")
   lines(x[1]+c(0, xrange), rep(mean(theory), 2), col='blue', lwd=3)
   lines(x, calc, col='red')
   legend("top", lwd=1, col=c("black","red"),
           legend=c("theory", "calc"))


*/

SEXP running_derivative(SEXP x, SEXP y, SEXP xrange)
{
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(xrange = AS_NUMERIC(xrange));
  int nx = LENGTH(x);
  int ny = LENGTH(y);
  if (nx != ny)
    error("length(x) is %d but length(y) is %d\n", nx, ny);
  double *xp = REAL(x);
  double *yp = REAL(y);
  int nxrange = LENGTH(xrange);
  double *xrangep = REAL(xrange);
#ifdef DEBUG
  Rprintf("nx: %d, ny: %d, nxrange: %d\n", nx, ny, nxrange);
#endif
  SEXP res;
  PROTECT(res = allocVector(REALSXP, nx));
  double *resp = REAL(res);
  if (1 == length(xrange)) {
    // formula: sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
    for (int i = 0; i < nx; i++) {
      double xsum = 0.0, ysum = 0.0;
      int n = 0;
      for (int j=0; j < nx; j++) {
	if (abs(xp[i] - xp[j]) < *xrangep) {
	  xsum += xp[j];
	  ysum += yp[j];
	  n++;
	}
      }
      double xbar = xsum / n, ybar = ysum / n;
#ifdef DEBUG
      Rprintf("xp[%d]: %f, xbar: %f, ybar: %f\n", i, xp[i], xbar, ybar);
#endif
      double Sxy = 0, Sxx = 0.0;
      for (int j=0; j < nx; j++) {
	if (abs(xp[i] - xp[j]) < *xrangep) {
	  Sxy += (xp[j] - xbar) * (yp[j] - ybar);
	  Sxx += (xp[j] - xbar) * (xp[j] - xbar);
	}
      }
      resp[i] = Sxy / Sxx;
    } // for (int i...)
  } else {
    error("at least for now, length(xrange) must be 1\n");
  }
  UNPROTECT(4);
  return(res);
}

