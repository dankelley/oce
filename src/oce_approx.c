/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

//#define DEBUG
//#define DEBUG_INTERP

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define SQR(x) ((x) * (x))

/*

   To test this, without building the whole package, do the following.

   In R:

   par(mar=c(2,2,1,1))
   library(oce)
   data(RRprofile)
   zz <- seq(0,2000,5)


   x <- c(0, 1, 2, 3, 3, 4, 5,  6,  6,  7,   8,   9)
   y <- c(0, 0, 0, 0, 0, 0, 1, 10, 10, 80, 100, 150)

   system("R CMD SHLIB oce_approx.c");dyn.load("oce_approx.so");.Call("oce_approx", x, y, x)

   TT <- .Call("oce_approx",RRprofile$depth,RRprofile$temperature,zz)
   plot(RRprofile$temperature,RRprofile$depth,ylim=c(500,0),xlim=c(2,10), col='red', pch=20)
   lines(TT,zz,col='blue')

*/

//

// x is a point, while xx and yy are arrays of length 4.
static double interp(double *xoutp, double *xp, double *yp, int i, int j, int ok[4])
{
  // Handle case without 4 neighbors.  It's not clear from the NODC
  // documents just how to do this, in the case where there are under
  // three local points
  double x = xoutp[i]; 
  double xx[4], yy[4];
  int ngood = 0;
  for (int n = 0; n < 4; n++) {
    xx[n] = yy[n] = 999.0;
    if (ok[n]) {
      xx[ngood] = xp[j + n - 1];
      yy[ngood] = yp[j + n - 1];
      ngood++;
    }
  }
#ifdef DEBUG_INTERP
  Rprintf("# ngood: %d\n", ngood);
#endif
  if (ngood < 2)
    return(NA_REAL);
  // Below is standard lagrangian interpolation.  FIXME: should
  // check that den != 0;
  double y = 0.0;
  for (int l = 0; l < ngood; l++) {
    double num = 1.0, den = 1.0;
    for (int m = 0; m < ngood; m++) {
      if (m != l) {
        num *= x - xx[m];
        den *= xx[l] - xx[m];
      }
    }
    y += yy[l] * num / den;
  }
#ifdef DEBUG_INTERP
  Rprintf("library(polynom)\n");
  Rprintf("x<-%.6f; y<-%.6f; xx<-c(%.6f,%.6f,%.6f,%.6f); yy<-c(%.6f,%.6f,%.6f,%.6f); xx<-xx[xx!=999]; yy<-yy[yy!=999]; plot(xx,yy, type='o');points(x,y,col='red')\n",
      x, y,
      xx[0], xx[1], xx[2], xx[3],
      yy[0], yy[1], yy[2], yy[3]);
  Rprintf("y - predict(poly.calc(xx, yy), x)\n");
#endif
  return(y);
}

// returns pointer to int array telling if values are in fence
int fok[4];
void fence(double *xoutp, double *xp, int i, int j, int x_len)
{
  if (j < 1 || j >= (x_len - 2)) {
    for (int i = 0; i < 4; i++)
      fok[i] = 0;
  } else {
    double xout = xoutp[i];
    // af=above-far, an=above-near, bn=below-near, bf=below-far
    double xan = xp[j], xbn = xp[j+1];
    // Inner neighbors must be within
    //   5m for data above 10m
    //   50m above 250m
    //   100m above 900m
    //   200m above 2000m
    //   1000m otherwise.
    if (xout < 10) {
      fok[0] = fabs(xout - xan) < 5;
      fok[1] = fabs(xout - xbn) < 5;
    } else if (xout < 250) {
      fok[0] = fabs(xout - xan) < 50;
      fok[1] = fabs(xout - xbn) < 50;
    } else if (xout < 900) {
      fok[0] = fabs(xout - xan) < 100;
      fok[1] = fabs(xout - xbn) < 100;
      //if (i == 48) Rprintf("xan:%.1f (%.1f) xbn:%.1f\n %.1f (0) %.1f\nfok[0]:%d fok[1]:%d\n", xan, xout, xbn, fabs(xout - xan), fabs(xout - xbn), fok[0], fok[1]);
    } else if (xout < 2000) {
      fok[0] = fabs(xout - xan) < 200;
      fok[1] = fabs(xout - xbn) < 200;
    } else {
      fok[0] = fabs(xout - xan) < 1000;
      fok[1] = fabs(xout - xbn) < 1000;
    }
    // Outer neighbors must be within
    //   200m above 500m
    //   400m above 1300m
    //   1000m otherwise
    if (xout < 500) {
      fok[2] = fabs(xout - xan) < 200;
      fok[3] = fabs(xout - xbn) < 200;
    } else if (xout < 130) {
      fok[2] = fabs(xout - xan) < 400;
      fok[3] = fabs(xout - xbn) < 400;
    } else {
      fok[2] = fabs(xout - xan) < 1000;
      fok[3] = fabs(xout - xbn) < 1000;
    }
  }
} 

int between(double x, double x0, double x1)
{
  int rval = 0;
  if (x0 == x1) {
    rval = (x == x0);
  } else if (x0 < x1) {
    rval = (x0 <= x && x <= x1);
  } else {
    rval = (x1 <= x && x <= x0);
  }
  return(rval);
}
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
#ifdef DEBUG
    if (i0 == 1) Rprintf("phi_z(2, ...): phiR %f   phiP1 %f    phiP2 %f\n", phiR, phiP1, phiP2);
#endif
    if (z0 < *(z + i0    )) error("z0=%f must equal or exceed z[i0=%d]=%f\n", z0, i0, *(z + i0));
    if (z0 > *(z + i0 + 1)) error("z0=%f must equal or be smaller than [(i0+1)=%d]=%f\n", z0, i0+1, *(z + i0+1));
    double denom = fabs(phiR - phiP1) + fabs(phiR - phiP2);
    //return ((fabs(phiR - phiP1) * phiP2 + fabs(phiR - phiP2) * phiP1) / (fabs(phiR - phiP1) + fabs(phiR - phiP2)));
    if (0.0 == denom)
      return (phiP2); // FIXME: is this a reasonable thing to return?
    else
      return ((fabs(phiR - phiP1) * phiP2 + fabs(phiR - phiP2) * phiP1) / denom);
  } else {
    error("phi_z given bad i0=%d (not in range 1 to %d)", i0, len-1);
    return (0.0); // never reached
  }
} // phi_z

static double phi_R(int i0, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3a) */
{
#ifdef DEBUG
  Rprintf("phi_R ...\n");
#endif
  if (0 < i0 && i0 < (len - 2)) {
    double phi12 = phi_ij(i0-1, i0  , z0, z, phi, len);
    double phi23 = phi_ij(i0  , i0+1, z0, z, phi, len);
    double phi34 = phi_ij(i0+1, i0+2, z0, z, phi, len);
    double denom = SQR(phi23 - phi34) + SQR(phi12 - phi23);
#ifdef DEBUG
    Rprintf("i0=%d phi_R phi12=%f phi23=%f phi34=%f numer=%f denom=%f BAD? %d\n",
        i0, phi12, phi23, phi34,
        SQR(phi23 - phi34) * phi12 + SQR(phi12 - phi23) * phi34,
        SQR(phi23 - phi34) + SQR(phi12 - phi23),
        0.0==denom);
    if (0.0 == denom) Rprintf("zero denom\n");
#endif
    if (denom != 0)
      return (0.5 * (phi23 + (SQR(phi23 - phi34) * phi12 + SQR(phi12 - phi23) * phi34) / denom));
    else
      return (0.5 * (phi23));
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
#ifdef DEBUG
    Rprintf("gamma_ijk(i=%d, j=%d, k=%d, ...) has denom=%f\n", i, j, k, ((z[i] - z[j]) * (z[i] - z[k])));
#endif
    return ((z0 - z[j]) * (z0 - z[k])) / ((z[i] - z[j]) * (z[i] - z[k]));
  } else {
    error("gamma_ijk given bad i=%d or bad j=%d or bad k=%d (with len=%d)", i, j, k, len);
    return (0.0); // never reached
  }
}
static double phi_ij(int i, int j, double z0, double *z, double *phi, int len) /* Reiniger & Ross (1968, eqn 3d) */
{
  if (-1 < i && i < len && -1 < j && j < len) {
#ifdef DEBUG
    Rprintf("  phi_ij(i=%d, j=%d, z0=%f, ...) has denom=%f\n", i, j, z0, (z[i] - z[j]));
#endif
    return (phi[i] * (z0 - z[j]) - phi[j] * (z0 - z[i])) / (z[i] - z[j]);
  } else {
    error("phi_ij given bad i=%d or bad j=%d (with len=%d)", i, j, len);
    return (0.0); // never reached
  }
}

SEXP oce_approx(SEXP x, SEXP y, SEXP xout, SEXP method) // , SEXP n, SEXP m)
{
  int x_len = length(x);
  int  y_len = length(y);
  int xout_len = length(xout);
  double *xp, *yp, *xoutp, *ansp;
  SEXP ans;
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(xout = AS_NUMERIC(xout));
  PROTECT(method = AS_NUMERIC(method));
  const int Method = (int)floor(0.5 + *REAL(method));
  if (Method != 1 && Method != 2)
    error("method must be 'nodc' or 'rr'");
  if (x_len != y_len) error("lengths of x (%d) and y (%d) disagree", x_len, y_len);
  xp = REAL(x);
  xoutp = REAL(xout);
  yp = REAL(y);
  PROTECT(ans = allocVector(REALSXP, xout_len));
  ansp = REAL(ans);
  //#ifdef DEBUG
  //  Rprintf("DEBUG: x="); for (int i = 0; i < x_len; i++) Rprintf("%f ", *(xp + i));  Rprintf("\n");
  //  Rprintf("DEBUG: y="); for (int i = 0; i < x_len; i++) Rprintf("%f ", *(yp + i));  Rprintf("\n");
  //  Rprintf("DEBUG: xout="); for (int i = 0; i < xout_len; i++) Rprintf("%f ", *(xoutp + i));  Rprintf("\n");
  //#endif
  //
#ifdef DEBUG
    Rprintf("Method:%d\n", Method);
#endif
  for (int i = 0; i < xout_len; i++) {
    double val = 0.0; // value always altered; this is to prevent compiler warning
    int found;
    found = 0;
#ifdef DEBUG
    Rprintf("xoutp[%d]:%f, xp[0]:%f\n", i, xoutp[i], xp[0]);
#endif
    // Handle top region (above 5m)
    if (Method == 1 && (xoutp[i] <= xp[0] && xp[0] <= 5)) {
      val = yp[0];
      found = 1;
      //#ifdef DEBUG_INTERPOLATION
      //      Rprintf("# xoutp[%d]=%.1f is above 5m, setting to xp[0]=%.1f\n", i, xoutp[i], val);
      //#endif
    } else {
      // Handle region below 5m, by finding j such that xp[j] <= xout[i] <= xp[j+1]
      for (int j = 0; j < x_len - 1; j++) {
#ifdef DEBUG
        //Rprintf("x[%d]=%.1f\n", j, *(xp + j));
#endif
        double xx = xoutp[i];
        //Rprintf("xoutp[%d]:%f, xp[%d]:%.1f, Method:%d\n", i, xoutp[i], j, xp[j], Method);
        // Look for neighbors
        if (xx == xp[j]) {
          // Exact match with point above
          val = yp[j];
          found = 1;
#ifdef DEBUG
          Rprintf("i=%d j=%d exact match with point above\n", i, j);
#endif
        } else if (xx == xp[j + 1]) {
          // Exact match with point below
          val = yp[j + 1];
          found = 1;
#ifdef DEBUG
          Rprintf("i=%d j=%d exact match with point below\n", i, j);
#endif
        } else if (xp[j] < xx && xx < xp[j + 1]) {
#ifdef DEBUG
          Rprintf("i=%d j=%d has a neighbor above and below\n", i, j);
#endif
          // Has a neeighbor above and below
          if (j == 0) {           /* catch exact match (just in case there is a problem with such) */
            val = yp[0] + (xx - xp[0]) * (yp[1] - yp[0]) / (xp[1] - xp[0]);
#ifdef DEBUG
            Rprintf("j=0 ... xx=%f yields val=%f since x[0,1]=%f , %f have y[0,1]=%f , %f\n",
                xx, val, xp[0], xp[1], yp[0], yp[1]);
#endif
          } else if (j == x_len - 1) {
            val = yp[j - 1] + (xx - xp[j - 1]) * (yp[j] - yp[j - 1]) / (xp[j] - xp[j - 1]);
          } else {
            if (j >= x_len - 2) {
              //Rprintf("j >= x_len - 2\n");
              //val = NA_REAL;
              val = yp[x_len - 1]; // trim to endpoint
            } else {
              //if (j > 2 && j < (xout_len - 2)) {
              //  Rprintf("xout[i=%d]:%.1f, xp[j-1]:%.1f xp[j=%d]:%.1f | xp[j+1]:%.1f xp[j+2]:%.1f\n",
              //      i, xoutp[i], xp[j-1], j, xp[j], xp[j+1], xp[j+2]);
              //}
              val = phi_z(j, xx, xp, yp, x_len);
              if (Method == 1) {
                fence(xoutp, xp, i, j, x_len);
                if (4 != fok[0] + fok[1] + fok[2] + fok[3]) {
#ifdef DEBUG_INTERP
                  Rprintf("# using 3-point lagrangian interpolation at i:%d, fok: %d %d %d %d\n",
                      i, fok[0], fok[1], fok[2], fok[3]);
#endif
                  val = interp(xoutp, xp, yp, i, j, fok);
                }
                //Rprintf("xoutp[%d]:%.1f, j:%d, val:%.1f, xp[j-1]:%.1f, xp[j]:%.1f, xp[j+1]:%.1f, yp[j]:%.1f, yp[j+1]:%.1f\n",
                //    i, xoutp[i], j, val, xp[j-1], xp[j], xp[j+1], yp[j], yp[j+1]);
                if (!between(val, yp[j], yp[j+1])) {
                  val = yp[j] + (xoutp[i] - xp[j]) * (yp[j+1] - yp[j]) / (xp[j+1] - xp[j]);
#ifdef DEBUG_INTERP
                  Rprintf("# using linear interp at xout[%d]=%.1f since %.1f is not bounded by %.1f and %.1f\n",
                      i, xoutp[i], val, yp[j], yp[j+1]);
#endif
                }
              }
            }
          }
#ifdef DEBUG
          Rprintf("oce_approx() got rval[%d] = %f\n\n", i, val);
#endif
          found = 1;
          break;
        }
      }
    }
    ansp[i] = found?val:NA_REAL;
  }
  UNPROTECT(5);
  return(ans);
}
