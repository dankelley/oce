/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rinternals.h>

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

static double interpolate_barnes(double xx, double yy, double zz, /* interpolate to get zz value at (xx,yy) */
    int skip, /* value in (x,y,z) to skip, or -1 if no skipping */
    unsigned int n, double *x, double *y, double *z, double *w, /* data num, locations, values, weights */
    double *z_last, /* last estimate of z at (x,y) */
    double xr, double yr) /* influence radii */
{
  double sum = 0.0, sum_w = 0.0, dx, dy, d, weight;
  for (int k = 0; k < n; k++) {
    //if (ISNA(x[k]) || ISNA(y[k]) || ISNA(z[k]) || ISNA(w[k])) continue;
    if (k != skip) {
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
  double sum_w = 0.0, dx, dy, d, weight;
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
    SEXP xg, SEXP yg,		   /* grid */
    SEXP xr, SEXP yr, /* influence radii */
    SEXP gamma,	     /* radius-reduction factor */
    SEXP iterations) /* number of iterations */
{
  double *rx, *ry, *rz, *rw; /* data */
  double *rxg, *ryg;	   /* grid */
  double *rxr, *ryr;	   /* radii */
  double *rgamma;		   /* gamma */
  int *niter;		   /* num iterations */
  int nx, nxg, nyg, i, j, k, it;
  double *z_last;
  double *zz, *nn;	  /* matrices */
  double *rzg;		  /* resultant matrix of interpolated value at grid*/
  double *rwg;		  /* resultant matrix of weight at grid */
  double *rzd;		  /* resultant vector of interpolated value at data */
  double xr2, yr2;	  /* radii, adjusting */
  SEXP zg, wg, zd;
  SEXP rval, rval_names;
  nx = length(x);
  nxg = length(xg);
  nyg = length(yg);
  PROTECT(rval = allocVector(VECSXP, 3));
  PROTECT(rval_names = allocVector(STRSXP, 3));
  PROTECT(zg = allocMatrix(REALSXP, nxg, nyg));
  PROTECT(wg = allocMatrix(REALSXP, nxg, nyg));
  PROTECT(zd = allocVector(REALSXP, nx));

  rx = REAL(x);
  ry = REAL(y);
  rz = REAL(z);
  rw = REAL(w);
  
  rxg = REAL(xg);
  ryg = REAL(yg);
  rgamma = REAL(gamma);
  niter = INTEGER(iterations);
  if (*niter < 0) error("cannot have a negative number of iterations");
  if (*niter > 20) error("cannot have more than 20 iterations.  Got %d", *niter);
  rxr = REAL(xr);
  ryr = REAL(yr);
  xr2 = *rxr;
  yr2 = *ryr;

  /* previous values */
  z_last = (double*)R_alloc(nx, sizeof(double));
  /* working matrices */
  zz = (double*)R_alloc(nxg * nyg, sizeof(double));
  nn = (double*)R_alloc(nxg * nyg, sizeof(double));

  rzg = REAL(zg);
  rwg = REAL(wg);
  rzd = REAL(zd);  // used to be called z_last2

  for (i = 0; i < nxg; i++)
    for (j = 0; j < nyg; j++) 
      zz[i + nxg*j] = 0.0;
  for (k = 0; k < nx; k++)
    z_last[k] = rzd[k] = 0.0;
  for (it = 0; it < *niter; it++) {
    /* update grid */
    for (i = 0; i < nxg; i++) {
      for (j = 0; j < nyg; j++) {
	zz[i + nxg*j] = interpolate_barnes(rxg[i], ryg[j], zz[i + nxg*j],
	    -1, /* no skip */
	    nx,
	    rx, ry, rz, rw,
	    z_last,
	    xr2, yr2);
      }
      R_CheckUserInterrupt();
    }
    /* interpolate grid back to data locations */
    for (k = 0; k < nx; k++) {
      rzd[k] = interpolate_barnes(rx[k], ry[k], z_last[k],
	  -1, /* BUG: why not skip? */
	  nx,
	  rx, ry, rz, rw,
	  z_last,
	  xr2, yr2);
    }
    R_CheckUserInterrupt();
    for (k = 0; k < nx; k++)
      z_last[k] = rzd[k];
    if (*rgamma > 0.0) {
      /* refine search range for next iteration */
      xr2 *= sqrt(*rgamma);
      yr2 *= sqrt(*rgamma);
    }
  }

  // copy matrix to return value
  for (i = 0; i < nxg; i++) {
    for (j = 0; j < nyg; j++) {
      rzg[i + nxg * j] = zz[i + nxg * j];
    }
    R_CheckUserInterrupt();
  }

  // weights at final region-of-influence radii
  for (i = 0; i < nxg; i++) {
    for (j = 0; j < nyg; j++) {
      rwg[i + nxg*j] = weight_barnes(rxg[i], ryg[j],
	  -1, /* no skip */
	  nx,
	  rx, ry, rz, rw,
	  xr2, yr2);
    }
    R_CheckUserInterrupt();
  }

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
