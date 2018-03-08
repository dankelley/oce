/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <Rcpp.h>
using namespace Rcpp;

#define SQR(x) ((x) * (x))
//#define DEBUG
//#define DEBUG_INTERP

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
void fence(double *xoutp, double *xp, int i, int j, int nx)
{
  if (j < 1 || j >= (nx - 2)) {
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
    if (z0 < *(z + i0    )) ::Rf_error("z0=%f must equal or exceed z[i0=%d]=%f\n", z0, i0, *(z + i0));
    if (z0 > *(z + i0 + 1)) ::Rf_error("z0=%f must equal or be smaller than [(i0+1)=%d]=%f\n", z0, i0+1, *(z + i0+1));
    double denom = fabs(phiR - phiP1) + fabs(phiR - phiP2);
    //return ((fabs(phiR - phiP1) * phiP2 + fabs(phiR - phiP2) * phiP1) / (fabs(phiR - phiP1) + fabs(phiR - phiP2)));
    if (0.0 == denom)
      return (phiP2); // FIXME: is this a reasonable thing to return?
    else
      return ((fabs(phiR - phiP1) * phiP2 + fabs(phiR - phiP2) * phiP1) / denom);
  } else {
    ::Rf_error("phi_z given bad i0=%d (not in range 1 to %d)", i0, len-1);
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
    ::Rf_error("phi_R given bad i0=%d (note that len=%d)", i0, len);
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
    ::Rf_error("phi_P1 given bad i0=%d", i0);
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
    ::Rf_error("phi_P2 given bad i0=%d", i0);
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
    ::Rf_error("gamma_ijk given bad i=%d or bad j=%d or bad k=%d (with len=%d)", i, j, k, len);
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
    ::Rf_error("phi_ij given bad i=%d or bad j=%d (with len=%d)", i, j, len);
    return (0.0); // never reached
  }
}

// Cross-reference work:
// 1. update ../src/registerDynamicSymbol.c with an item for this
// 2. main code should use the autogenerated wrapper in ../R/RcppExports.R
//
// [[Rcpp::export]]
NumericVector do_oceApprox(NumericVector x, NumericVector y, NumericVector xout, NumericVector method)
{
  int nx = x.size();
  int ny = y.size();
  int nxout = xout.size();
  double *xp = &x[0];
  double *xoutp = &xout[0];
  double *yp = &y[0];
  NumericVector ans(nxout);
  const int Method = (int)floor(0.5 + method[0]);
  if (Method != 1 && Method != 2)
    ::Rf_error("method must be 'nodc' or 'rr'");
  if (nx != ny) ::Rf_error("lengths of x (%d) and y (%d) disagree", nx, ny);
#ifdef DEBUG
    Rprintf("Method:%d\n", Method);
#endif
  for (int i = 0; i < nxout; i++) {
    double val = 0.0; // value always altered; this is to prevent compiler warning
    int found;
    found = 0;
#ifdef DEBUG
    Rprintf("xout[%d]:%f, x[0]:%f\n", i, xout[i], x[0]);
#endif
    // Handle top region (above 5m)
    if (Method == 1 && (xout[i] <= x[0] && x[0] <= 5)) {
      val = y[0];
      found = 1;
      //#ifdef DEBUG_INTERPOLATION
      //      Rprintf("# xoutp[%d]=%.1f is above 5m, setting to xp[0]=%.1f\n", i, xoutp[i], val);
      //#endif
    } else {
      // Handle region below 5m, by finding j such that xp[j] <= xout[i] <= xp[j+1]
      for (int j = 0; j < nx - 1; j++) {
#ifdef DEBUG
        //Rprintf("x[%d]=%.1f\n", j, *(xp + j));
#endif
        double xx = xout[i];
        //Rprintf("xoutp[%d]:%f, xp[%d]:%.1f, Method:%d\n", i, xoutp[i], j, xp[j], Method);
        // Look for neighbors
        if (xx == x[j]) {
          // Exact match with point above
          val = y[j];
          found = 1;
#ifdef DEBUG
          Rprintf("i=%d j=%d exact match with point above\n", i, j);
#endif
        } else if (xx == x[j + 1]) {
          // Exact match with point below
          val = y[j + 1];
          found = 1;
#ifdef DEBUG
          Rprintf("i=%d j=%d exact match with point below\n", i, j);
#endif
        } else if (x[j] < xx && xx < x[j + 1]) {
#ifdef DEBUG
          Rprintf("i=%d j=%d has a neighbor above and below\n", i, j);
#endif
          // Has a neeighbor above and below
          if (j == 0) {           /* catch exact match (just in case there is a problem with such) */
            val = y[0] + (xx - x[0]) * (y[1] - y[0]) / (x[1] - x[0]);
#ifdef DEBUG
            Rprintf("j=0 ... xx=%f yields val=%f since x[0,1]=%f , %f have y[0,1]=%f , %f\n",
                xx, val, x[0], x[1], y[0], y[1]);
#endif
          } else if (j == nx - 1) {
            val = y[j - 1] + (xx - x[j - 1]) * (y[j] - y[j - 1]) / (x[j] - x[j - 1]);
          } else {
            if (j >= nx - 2) {
              //Rprintf("j >= nx - 2\n");
              //val = NA_REAL;
              val = yp[nx - 1]; // trim to endpoint
            } else {
              //if (j > 2 && j < (xout_len - 2)) {
              //  Rprintf("xout[i=%d]:%.1f, xp[j-1]:%.1f xp[j=%d]:%.1f | xp[j+1]:%.1f xp[j+2]:%.1f\n",
              //      i, xoutp[i], xp[j-1], j, xp[j], xp[j+1], xp[j+2]);
              //}
              val = phi_z(j, xx, xp, yp, nx);
              if (Method == 1) {
                fence(xoutp, xp, i, j, nx);
                if (4 != fok[0] + fok[1] + fok[2] + fok[3]) {
#ifdef DEBUG_INTERP
                  Rprintf("# using 3-point lagrangian interpolation at i:%d, fok: %d %d %d %d\n",
                      i, fok[0], fok[1], fok[2], fok[3]);
#endif
                  val = interp(xoutp, xp, yp, i, j, fok);
                }
                //Rprintf("xout[%d]:%.1f, j:%d, val:%.1f, x[j-1]:%.1f, x[j]:%.1f, x[j+1]:%.1f, y[j]:%.1f, y[j+1]:%.1f\n",
                //    i, xout[i], j, val, x[j-1], x[j], x[j+1], y[j], y[j+1]);
                if (!between(val, y[j], y[j+1])) {
                  val = y[j] + (xout[i] - x[j]) * (y[j+1] - y[j]) / (x[j+1] - x[j]);
#ifdef DEBUG_INTERP
                  Rprintf("# using linear interp at xout[%d]=%.1f since %.1f is not bounded by %.1f and %.1f\n",
                      i, xout[i], val, y[j], y[j+1]);
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
    ans[i] = found?val:NA_REAL;
  }
  return(ans);
}
