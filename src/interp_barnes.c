#include <R.h>
#include <Rinternals.h>

/*
# R CMD SHLIB interp_barnes.c
data(wind)
x <- wind$x; y <- wind$y; u <- wind$u+0.0
w <- rep(1.0, length(x))
xg <- seq(0, 12, 0.25)
yg <- seq(0, 10, 0.25)
xr <- 2.71563
yr <- 2.01158
gamma <- 0.5
niter <- 2
load("~/t.rda")
#dyn.load("interp_barnes.so")
g <- .Call("interp_barnes", x, y, u, w, xg, yg, xr, yr, gamma, as.integer(niter))
contour(xg, yg, g)
points(x,y,col=hsv(0.666*(u-min(u))/diff(range(u)),1,1),pch=20)
*/

static double
interpolate_barnes(double xx, double yy, double zz, /* interpolate to get zz value at (xx,yy) */
		   int skip, /* value in (x,y,z) to skip, or -1 if no skipping */
		   unsigned int n, double *x, double *y, double *z, double *w, /* data num, locations, values, weights */
		   double *z_last, /* last estimate of z at (x,y) */
		   double xr, double yr) /* influence radii */
{
	double sum = 0.0, sum_w = 0.0, dx, dy, d, weight;
	int k;
	for (k = 0; k < n; k++) {
		if (k != skip) {
			dx = (xx - x[k]) / xr;
			dy = (yy - y[k]) / yr;
			d = dx*dx + dy*dy;
			weight = w[k] * exp(-d);
			sum += weight * (z[k] - z_last[k]);
			sum_w += weight;
		}
	}
	return ((sum_w > 0.0) ? (zz + sum / sum_w) : NA_REAL);
}


SEXP interp_barnes(SEXP x, SEXP y, SEXP z, SEXP w, /* z at (x,y), weighted by w */
		   SEXP xg, SEXP yg,		   /* grid */
		   SEXP xr, SEXP yr, /* influence radii */
		   SEXP gamma,	     /* radius-reduction factor */
		   SEXP iterations)  /* number of iterations */
{
	double *rx, *ry, *rz, *rw; /* data */
	double *rxg, *ryg;	   /* grid */
	double *rxr, *ryr;	   /* radii */
	double *rgamma;		   /* gamma */
	int *niter;		   /* num iterations */
	int nx, nxg, nyg, i, j, k, it;
	double *z_last, *z_last2, *zz; /* previous values */
	double *rans;		  /* result */
	double xr2, yr2;	  /* radii, adjusting */
	SEXP ans;
	
	nx = length(x);
	rx = REAL(x); ry = REAL(y); rz = REAL(z); rw = REAL(w);
	
	nxg = length(xg);
	nyg = length(yg);
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
	z_last = (double*)malloc(nx * sizeof(double));
	z_last2 = (double*)malloc(nx * sizeof(double));
	zz = (double*)malloc(nxg * nyg * sizeof(double));

	for (i = 0; i < nxg; i++)
		for (j = 0; j < nyg; j++) 
			zz[i + nxg*j] = 0.0;
	for (k = 0; k < nx; k++)
		z_last[k] = z_last2[k] = 0.0;
	PROTECT(ans = allocMatrix(REALSXP, nxg, nyg));
	rans = REAL(ans);
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
		}
		/* interpolate grid back to data locations */
		for (k = 0; k < nx; k++) {
			z_last2[k] = interpolate_barnes(rx[k], ry[k], z_last[k],
							-1, /* BUG: why not skip? */
							nx,
							rx, ry, rz, rw,
							z_last,
							xr2, yr2);
		}
		for (k = 0; k < nx; k++)
			z_last[k] = z_last2[k];
		if (*rgamma > 0.0) {
			/* refine search range for next iteration */
			xr2 *= sqrt(*rgamma);
			yr2 *= sqrt(*rgamma);
		}
	}
	for (i = 0; i < nxg; i++) 
		for (j = 0; j < nyg; j++) 
			rans[i + nxg * j] = zz[i + nxg * j];
	free(z_last);
	free(z_last2);
	free(zz);
	UNPROTECT(1);
	return(ans);
}
