#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define SQR(x) (x * x)

/*

In shell:
R CMD SHLIB misc.c

In R:
dyn.load("misc.so");x<-1:3;y<-x*3;xout<-1:10;.Call("oce_approx",x,y,xout,1,2)

dyn.load("misc.so");x<-1:10;y<-x*3;xout<-seq(2,9,0.1);p<-.Call("oce_approx",x,y,xout,1,2);plot(x,y);lines(xout,p)

In shell:
R CMD SHLIB misc.c
# must be a bug, still, since getting odd wiggles in some places.
# suspect either typo or problem in inequalities (check matlab for latter)
par(mar=c(2,2,1,1))
library(oce)
data(RRprofile)
zz<-seq(0,2000,100)
dyn.load("misc.so")
TT<-.Call("oce_approx",RRprofile$depth,RRprofile$temperature,zz,1,2)
plot(RRprofile$temperature,RRprofile$depth)
lines(TT,zz,col='red')

*/
static double gamma_ijk(int i, int j, int k, double z0, double *z, int len) /* Reiniger & Ross (1968, eqn 3c) */
{
  if (-1 < i && -1 < j && -1 < k && i < len && j < len && k < len) {
    //Rprintf("gamma_ijk denom=%f\n", ((z[i] - z[j]) * (z[i] - z[k])));
    return ((z0 - z[j]) * (z0 - z[k])) / ((z[i] - z[j]) * (z[i] - z[k]));
  } else
    error("gamma_ijk given bad i=%d or bad j=%d or bad k=%d (with len=%d)", i, j, k, len);
}
static double phi_ij(int i, int j, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3d) */
{
  if (-1 < i && i < len && -1 < j && j < len) {
    //Rprintf("phi_ij denom=%f\n", (z[i] - z[j]));
    return (phi[i] * (z0 - z[j]) - phi[j] * (z0 - z[i])) / (z[i] - z[j]);
  } else
    error("phi_ij given bad i=%d or bad j=%d (with len=%d)", i, j, len);
}
static double phi_P1(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3b.1) */
{
  if (1 < i0 && i0 < (len - 1))
    return 
      gamma_ijk(i0-2, i0-1, i0+1, z0, z, len) * phi[i0-2] + 
      gamma_ijk(i0-1, i0+1, i0-2, z0, z, len) * phi[i0-1] +
      gamma_ijk(i0+1, i0-2, i0-1, z0, z, len) * phi[i0+1];
  else
    error("phi_P1 given bad i0=%d", i0);
}
static double phi_P2(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3b.2) */
{
  if (1 < i0 && i0 < (len - 2))
    return 
      gamma_ijk(i0-1, i0+1, i0+2, z0, z, len) * phi[i0-1] +
      gamma_ijk(i0+1, i0+2, i0-1, z0, z, len) * phi[i0+1] +
      gamma_ijk(i0+2, i0-1, i0+1, z0, z, len) * phi[i0+2];
  else
    error("phi_P2 given bad i0=%d", i0);
}
static double phi_R(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3a) */
{
  if (1 < i0 && i0 < (len - 2)) {
    double phi12 = phi_ij(i0-2, i0-1, z0, z, phi, len);
    double phi23 = phi_ij(i0-1, i0+1, z0, z, phi, len);
    double phi34 = phi_ij(i0+1, i0+2, z0, z, phi, len);
    //Rprintf("phi_R denom=%f\n", (SQR(phi23 - phi34) + SQR(phi12 - phi23)));
    return 0.5 * (phi23 + 
                  (SQR(phi23 - phi34) * phi12 + SQR(phi12 - phi23) * phi34) 
                  /
                  (SQR(phi23 - phi34) + SQR(phi12 - phi23)));
  } else
    error("phi_R given bad i0=%d", i0);
}
double phi_z(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3) */
{
  if (1 < i0 && i0 < (len - 1)) {
    double phiR = phi_R(i0, z0, z, phi, len);
    double phiP1 = phi_P1(i0, z0, z, phi, len);
    double phiP2 = phi_P2(i0, z0, z, phi, len);
#if 0
    Rprintf("phi_z(i0=%d, z0=%f, ...) has phiR=%f  phiP1=%f  phiP2=%f  denom=%f and returns %f\n",
            i0,
            z0,
            phiR,
            phiP1,
            phiP2,
            (fabs(phiR - phiP1) + fabs(phiR - phiP2)),
            (fabs(phiR - phiP1) * phiP2 + fabs(phiR - phiP2) * phiP1)
            /
            (fabs(phiR - phiP1) + fabs(phiR - phiP2)));
#endif
    return
      (fabs(phiR - phiP1) * phiP2 + fabs(phiR - phiP2) * phiP1)
      /
      (fabs(phiR - phiP1) + fabs(phiR - phiP2));
  } else {
    error("phi_z given bad i0=%d", i0);
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
  int i;
#if 0
  Rprintf("DEBUG: x="); for (i = 0; i < x_len; i++) Rprintf("%f ", *(xp + i));  Rprintf("\n");
  Rprintf("DEBUG: y="); for (i = 0; i < x_len; i++) Rprintf("%f ", *(yp + i));  Rprintf("\n");
  Rprintf("DEBUG: xout="); for (i = 0; i < xout_len; i++) Rprintf("%f ", *(xoutp + i));  Rprintf("\n");
#endif
  for (i = 0; i < xout_len; i++) {
    //Rprintf("xout[%d] = %f\n",i,*(xoutp+i));
    int j;
    double val;
    int found;
    found = 0;
    //Rprintf("x[%d]=%.1f...", j, *(xp + j));
    for (j = 2; j < (x_len - 2); j++) {
      //Rprintf("%.1f", *(xp + j));
      // Look for neighbors (BUG: what about hitting directly?)
      if (*(xp + j) <= *(xoutp + i) && *(xoutp + i) < *(xp + j + 1)) {
        val = phi_z(j, *(xoutp + i), xp, yp, x_len);
        //Rprintf("Y j=%d VAL=%f\n", j, val);
        found = 1;
        break;
      } else {
        //Rprintf("N ");
      }
    }
    if (found) 
      *(ansp + i) = val;
    else 
      *(ansp + i) = 0.0;
  }
  UNPROTECT(4);
  return(ans);
}

/* 
** compile from commandline:
R CMD SHLIB test.c
** test R code:
m <- matrix(rep(seq(0, 1, length.out=5), 5), nrow=5, byrow=TRUE)
m[3,3] <- 2
dyn.load("test.so")
m1 <- .Call("matrix_smooth", m)
m2 <- .Call("matrix_smooth", m1)
m3 <- .Call("matrix_smooth", m2)
par(mfrow=c(2,2))
image(m,  col=rainbow(100), zlim=c(0,4), main="raw")
image(m1, col=rainbow(100), zlim=c(0,4), main="smoothed 1 time")
image(m2, col=rainbow(100), zlim=c(0,4), main="smoothed 2 times")
image(m3, col=rainbow(100), zlim=c(0,4), main="smoothed 3 times")
*/

SEXP matrix_smooth(SEXP mat)
{
  /* Note: the 2d data are stored in column order */
  SEXP res;
  int nrow = INTEGER(GET_DIM(mat))[0];
  int ncol = INTEGER(GET_DIM(mat))[1];
  int i, j;
  double *matp, *resp;
  if (!isMatrix(mat)) error("'mat' must be a matrix");
  //if (isInteger(mat)) warning("'mat' is integer, but should be real");
  if (!isReal(mat)) error("'mat' must be numeric, not integer");
  matp = REAL(mat);
  if (length(mat) != nrow * ncol) error("'nrow'*'ncol' must equal number of elements in 'mat'");
  PROTECT(res = allocMatrix(REALSXP, nrow, ncol));
  resp = REAL(res);
  // copy edges (change this, if filter size changes)
  for (j = 0; j < ncol; j++) {
    *(resp + j                    ) = *(matp + j                    );
    *(resp + j + ncol * (nrow - 1)) = *(matp + j + ncol * (nrow - 1));
  }
  for (i = 0; i < nrow; i++) {
    *(resp +      0     + ncol * i) = *(matp +      0     + ncol * i);
    *(resp + (nrow - 1) + ncol * i) = *(matp + (nrow - 1) + ncol * i);
  }
  // smooth middle
  for (i = 1; i < nrow - 1; i++) {
    for (j = 1; j < ncol - 1; j++) {
      *(resp + j + ncol * i) = 
	(2 * (*(matp +   j   + ncol *    i    )) +
	 1 * (*(matp + j - 1 + ncol *    i    )) +
	 1 * (*(matp + j + 1 + ncol *    i    )) +
	 1 * (*(matp +   j   + ncol * (i - 1) )) +
	 1 * (*(matp +   j   + ncol * (i + 1) ))) / 6.0;
    }
  }
  UNPROTECT(1);
  return(res);
}

#undef SQR
