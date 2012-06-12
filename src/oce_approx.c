/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define SQR(x) ((x) * (x))

/*

To test this, without building the whole package, do the following.

In shell:

R CMD SHLIB oce_approx.c

In R:

par(mar=c(2,2,1,1))
library(oce)
data(RRprofile)
zz <- seq(0,2000,5)
dyn.load("oce_approx.so")
TT <- .Call("oce_approx",RRprofile$depth,RRprofile$temperature,zz)
plot(RRprofile$temperature,RRprofile$depth,ylim=c(500,0),xlim=c(2,10), col='red', pch=20)
lines(TT,zz,col='blue')

*/

static double gamma_ijk(int i, int j, int k, double z0, double *z,              int len);
static double phi_ij(   int i, int j,        double z0, double *z, double *phi, int len);
static double phi_P1(int i0, double z0, double *z, double *phi, int len);
static double phi_P2(int i0, double z0, double *z, double *phi, int len);
static double phi_R( int i0, double z0, double *z, double *phi, int len);
static double phi_z( int i0, double z0, double *z, double *phi, int len);

static double phi_z(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3) */
{
  if (0 < i0 && i0 < (len - 1)) {
    double phiR = phi_R(i0, z0, z, phi, len);
    double phiP1 = phi_P1(i0, z0, z, phi, len);
    double phiP2 = phi_P2(i0, z0, z, phi, len);
    if (z0 < *(z + i0    )) error("z0=%f must equal or exceed z[i0=%d]=%f\n", z0, i0, *(z + i0));
    if (z0 > *(z + i0 + 1)) error("z0=%f must equal or be smaller than [(i0+1)=%d]=%f\n", z0, i0+1, *(z + i0+1));
    return (fabs(phiR - phiP1) * phiP2 + fabs(phiR - phiP2) * phiP1) / (fabs(phiR - phiP1) + fabs(phiR - phiP2));
  } else {
    error("phi_z given bad i0=%d (not in range 1 to %d)", i0, len-1);
    return (0.0); // never reached
  }
}
static double phi_R(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3a) */
{
  //Rprintf("phi_R ...\n");
  if (0 < i0 && i0 < (len - 2)) {
    double phi12 = phi_ij(i0-1, i0  , z0, z, phi, len);
    double phi23 = phi_ij(i0  , i0+1, z0, z, phi, len);
    double phi34 = phi_ij(i0+1, i0+2, z0, z, phi, len);
    //Rprintf("phi_R denom=%f\n", (SQR(phi23 - phi34) + SQR(phi12 - phi23)));
    return (0.5 * (phi23 + 
	  (SQR(phi23 - phi34) * phi12 + SQR(phi12 - phi23) * phi34)
	  /
	  (SQR(phi23 - phi34) + SQR(phi12 - phi23))));
  } else {
    error("phi_R given bad i0=%d (note that len=%d)", i0, len);
    return (0.0); // never reached
  }
}


static double phi_P1(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3b.1) */
{
  if (0 < i0 && i0 < (len - 1)) {
    return (gamma_ijk(i0-1, i0  , i0+1, z0, z, len) * phi[i0-1] + 
	gamma_ijk(i0  , i0+1, i0-1, z0, z, len) * phi[i0  ] +
	gamma_ijk(i0+1, i0-1, i0  , z0, z, len) * phi[i0+1]);
  } else {
    error("phi_P1 given bad i0=%d", i0);
    return (0.0); // never reached
  }
}
static double phi_P2(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3b.2) */
{
  if (0 < i0 && i0 < (len - 2)) {
    return (gamma_ijk(i0  , i0+1, i0+2, z0, z, len) * phi[i0  ] +
	gamma_ijk(i0+1, i0+2, i0  , z0, z, len) * phi[i0+1] +
	gamma_ijk(i0+2, i0  , i0+1, z0, z, len) * phi[i0+2]);
  } else {
    error("phi_P2 given bad i0=%d", i0);
    return (0.0); // never reached
  }
}
static double gamma_ijk(int i, int j, int k, double z0, double *z, int len) /* Reiniger & Ross (1968, eqn 3c) */
{
  if (-1 < i && -1 < j && -1 < k && i < len && j < len && k < len) {
    //Rprintf("gamma_ijk denom=%f\n", ((z[i] - z[j]) * (z[i] - z[k])));
    return ((z0 - z[j]) * (z0 - z[k])) / ((z[i] - z[j]) * (z[i] - z[k]));
  } else {
    error("gamma_ijk given bad i=%d or bad j=%d or bad k=%d (with len=%d)", i, j, k, len);
    return (0.0); // never reached
  }
}
static double phi_ij(int i, int j, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3d) */
{
  if (-1 < i && i < len && -1 < j && j < len) {
    //Rprintf("phi_ij denom=%f\n", (z[i] - z[j]));
    return (phi[i] * (z0 - z[j]) - phi[j] * (z0 - z[i])) / (z[i] - z[j]);
  } else {
    error("phi_ij given bad i=%d or bad j=%d (with len=%d)", i, j, len);
    return (0.0); // never reached
  }
}
SEXP oce_approx(SEXP x, SEXP y, SEXP xout, SEXP n, SEXP m)
{
  int x_len = length(x);
  int  y_len = length(y);
  int xout_len = length(xout);
  double *xp, *yp, *xoutp, *ansp;
  SEXP ans;
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(xout = AS_NUMERIC(xout));
  if (x_len != y_len) error("lengths of x (%d) and y (%d) disagree", x_len, y_len);
  xp = REAL(x);
  xoutp = REAL(xout);
  yp = REAL(y);
  PROTECT(ans = allocVector(REALSXP, xout_len));
  ansp = REAL(ans);
#if 0
  Rprintf("DEBUG: x="); for (int i = 0; i < x_len; i++) Rprintf("%f ", *(xp + i));  Rprintf("\n");
  Rprintf("DEBUG: y="); for (int i = 0; i < x_len; i++) Rprintf("%f ", *(yp + i));  Rprintf("\n");
  Rprintf("DEBUG: xout="); for (int i = 0; i < xout_len; i++) Rprintf("%f ", *(xoutp + i));  Rprintf("\n");
#endif
  for (int i = 0; i < xout_len; i++) {
    //Rprintf("xout[%d] = %f\n",i,*(xoutp+i));
    double val = 0.0; // value always altered; this is to prevent compiler warning
    int found;
    found = 0;
    for (int j = 0; j < x_len - 1; j++) {
      //Rprintf("x[%d]=%.1f\n", j, *(xp + j));
      double xx = *(xoutp + i);
      // Look for neighbors (BUG: what about hitting directly?)
      if (xx == *(xp + j)) {
        val = *(yp + j);
        found = 1;
      } else if (xx == *(xp + j + 1)) {
        val = *(yp + j + 1);
        found = 1;
      } else if (*(xp + j) < xx && xx < *(xp + j + 1)) {
        if (j == 0) {           /* catch exact match (just in case there is a problem with such) */
          val = *yp + (xx - *xp) * (*(yp + 1) - *(yp)) / (*(xp + 1) - *xp);
          //Rprintf("j=0 ... xx=%f yields val=%f since x[0,1]=%f , %f have y[0,1]=%f , %f\n", xx, val, *xp, *(xp+1), *yp,*(yp+1));
        } else if (j == x_len - 1) {
	  val = *(yp + j - 1) + (xx - *(xp + j - 1)) * (*(yp + j) - *(yp + j - 1)) / (*(xp + j) - *(xp + j - 1));
        } else {
	  if (j >= x_len - 2)
	    val = NA_REAL;
	  else
	    val = phi_z(j, xx, xp, yp, x_len);
        }
        //Rprintf("Y j=%d VAL=%f\n", j, val);
        found = 1;
        break;
      }
    }
    if (found) 
      *(ansp + i) = val;
    else 
      *(ansp + i) = NA_REAL;
  }
  UNPROTECT(4);
  return(ans);
}
