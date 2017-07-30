/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rinternals.h>
#include <time.h>
//#define DEBUG

/*
library(oce)
data(wind)
x <- wind$x; y <- wind$y; z <- wind$z+0.0
w <- rep(1.0, length(x))
xg <- seq(0, 12, 0.25)
yg <- seq(0, 10, 0.25)
xr <- 2.71563
yr <- 2.01158
gamma <- 0.5
niter <- 2
system("R CMD SHLIB interp_barnes.c")
dyn.load("interp_barnes.so")
g <- .Call("interp_barnes", x, y, z, w, xg, yg, xr, yr, gamma, as.integer(niter), as.integer(TRUE))
contour(xg, yg, g$zg)
text(x, y, round(z), col='blue')
*/

//#define USE_APPROX_EXP 1
#ifdef USE_APPROX_EXP
// Compute exp(-x) approximately, as efficiency measure.
// See Dan Kelley notebook [1997/1/25] for demonstration
// of factor of 3 speedup, with 1000 column data and a
// 10 by 10 grid, and demonstration that the
// error is < 0.1% in the final grid.
inline double exp_approx(double x)
{
  return 1.0 / (0.999448 
      + x * (1.023820 
        + x * (0.3613967
          + x * (0.4169646
            + x * (-0.1292509
              + x * 0.0499565)))));
}
#endif

static time_t start;

static double interpolate_barnes(double xx, double yy, double zz, /* interpolate to get zz value at (xx,yy) */
    int skip, /* value in (x,y,z) to skip, or -1 if no skipping */
    unsigned int nx, double *x, double *y, double *z, double *w, /* data num, locations, values, weights */
    double *z_last, /* last estimate of z at (x,y) */
    double xr, double yr, int debug) /* influence radii */
{
  double sum = 0.0, sum_w = 0.0;
  for (int k = 0; k < nx; k++) {
    // R trims NA (x,y values so no need to check here
    if (k != skip) {
      double dx, dy, d, weight;
      dx = (xx - x[k]) / xr;
      dy = (yy - y[k]) / yr;
      d = dx*dx + dy*dy;
#ifdef USE_APPROX_EXP
      weight = w[k] * exp_approx(-d);
#else
      weight = w[k] * exp(-d);
#endif
      sum += weight * (z[k] - z_last[k]);
      sum_w += weight;
    }
  }
  return ((sum_w > 0.0) ? (zz + sum / sum_w) : NA_REAL);
}

// next is modelled on interpolate_barnes()
static double weight_barnes(double xx, double yy,
    int skip,
    unsigned int n, double *x, double *y, double *z, double *w,
    double xr, double yr)
{
  double sum_w, dx, dy, d, weight;
  sum_w = 0.0;
  for (int k = 0; k < n; k++) {
    if (k != skip) {
      dx = (xx - x[k]) / xr;
      dy = (yy - y[k]) / yr;
      d = dx*dx + dy*dy;
#ifdef USE_APPROX_EXP
      weight = w[k] * exp_approx(-d);
#else
      weight = w[k] * exp(-d);
#endif
      sum_w += weight;
    }
  }
  return ((sum_w > 0.0) ? sum_w : NA_REAL);
}


SEXP interp_barnes(SEXP x, SEXP y, SEXP z, SEXP w, /* z at (x,y), weighted by w */
    SEXP xg, SEXP yg, /* grid */
    SEXP xr, SEXP yr, /* influence radii */
    SEXP gamma,       /* radius-reduction factor */
    SEXP iterations)  /* number of iterations */
{
  start = time(NULL);
  int nx = length(x);
  int nxg = length(xg);
  int nyg = length(yg);
  SEXP zg;
  PROTECT(zg = allocMatrix(REALSXP, nxg, nyg));
  SEXP wg;
  PROTECT(wg = allocMatrix(REALSXP, nxg, nyg));
  SEXP zd;
  PROTECT(zd = allocVector(REALSXP, nx));

  double *rx = REAL(x);  // x data
  double *ry = REAL(y);  // y data
  double *rz = REAL(z);  // z data
  double *rw = REAL(w);  // w (weight) data
  
  double *rxg = REAL(xg); // x grid
  double *ryg = REAL(yg); // y grid
  double *rgamma = REAL(gamma); // gamma
  int *niter = INTEGER(iterations); // number of iterations
  if (*niter < 0) error("cannot have a negative number of iterations.  Got %d ", *niter);
  if (*niter > 20) error("cannot have more than 20 iterations.  Got %d ", *niter);
  double *rxr = REAL(xr); // x radius
  double *ryr = REAL(yr); // y radius
  double xr2 = *rxr; // local radius, which will vary with iteration
  double yr2 = *ryr; // local radius, which will vary with iteration

  /* previous values */
  double *z_last = (double*)R_alloc((size_t)nx+100000, sizeof(double));
  /* working matrix */
  double *zz = (double*)R_alloc((size_t)(nxg * nyg)+100000, sizeof(double));

  double *rzg = REAL(zg);
  double *rwg = REAL(wg);
  double *rzd = REAL(zd);  // used to be called z_last2

  for (int ij = 0; ij < nxg * nyg; ij++)
    zz[ij] = 0.0;
  for (int k = 0; k < nx; k++) {
    z_last[k] = 0.0;
    rzd[k] = 0.0;
  }
  for (int iter = 0; iter < *niter; iter++) {
    /* update grid */
    for (int i = 0; i < nxg; i++) {
      for (int j = 0; j < nyg; j++) {
        zz[i + nxg*j] = interpolate_barnes(rxg[i], ryg[j], zz[i + nxg*j],
            -1, /* no skip */
            nx, rx, ry, rz, rw,
            z_last,
            xr2, yr2, i==(nxg-1)&&j==(nyg-1));
        R_CheckUserInterrupt();
      }
    }
    /* interpolate grid back to data locations */
    for (int k = 0; k < nx; k++) {
      rzd[k] = interpolate_barnes(rx[k], ry[k], z_last[k],
          -1, /* BUG: why not skip? */
          nx, rx, ry, rz, rw,
          z_last,
          xr2, yr2, 0);
    }
    R_CheckUserInterrupt();
    for (int k = 0; k < nx; k++)
      z_last[k] = rzd[k];
    if (*rgamma > 0.0) {
      // refine search range for next iteration
      xr2 *= sqrt(*rgamma);
      yr2 *= sqrt(*rgamma);
    }
  }

  // copy matrix to return value
  for (int ij = 0; ij < nxg * nyg; ij++) {
      rzg[ij] = zz[ij];
  }
  R_CheckUserInterrupt();

  // weights at final region-of-influence radii
  for (int i = 0; i < nxg; i++) {
    for (int j = 0; j < nyg; j++) {
      rwg[i + nxg*j] = weight_barnes(rxg[i], ryg[j],
          -1, /* no skip */
          nx, rx, ry, rz, rw,
          xr2, yr2);
    }
    R_CheckUserInterrupt();
  }

  // Prepare output list
  SEXP rval;
  PROTECT(rval = allocVector(VECSXP, 3));
  SEXP rval_names;
  PROTECT(rval_names = allocVector(STRSXP, 3));
  // zg = interpolated values at grid points (matrix)
  SET_VECTOR_ELT(rval, 0, zg);
  SET_STRING_ELT(rval_names, 0, mkChar("zg"));
  // wg = weights at grid points (matrix)
  SET_VECTOR_ELT(rval, 1, wg);
  SET_STRING_ELT(rval_names, 1, mkChar("wg"));
  // zd = interpolated values at data (vector of same length as x)
  SET_VECTOR_ELT(rval, 2, zd);
  SET_STRING_ELT(rval_names, 2, mkChar("zd")); 
  // save the names
  setAttrib(rval, R_NamesSymbol, rval_names);

  UNPROTECT(5);
  return(rval);
}
