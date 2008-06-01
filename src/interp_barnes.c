#include <R.h>
#include <Rinternals.h>

#define USE_APPROX_EXP 1

/*
# R CMD SHLIB barnes_interp.c
data(wind)
x <- wind$x; y <- wind$y; u <- wind$u
w <- rep(1.0, length(x))
xg <- seq(0, 12, 0.25)
yg <- seq(0, 10, 0.25)
xr <- 2.71563
yr <- 2.01158
gamma <- 0.5
niter <- 2
dyn.load("interp_barnes.so")
con <- .Call("interp_barnes", x, y, as.numeric(u), w, xg, yg, xr, yr, gamma, as.integer(niter))
contour(xg,yg,con)
points(x,y,col=hsv(0.666*(u-min(u))/diff(range(u)),1,1),pch=20)
*/

static double
interpolate_barnes(double xx, /* interpolate to get value at (xx,yy) */
		   double yy,	/* given previous value zz there */
		   double zz,	
		   int skip, /* value in (x,y,z) to skip, or -1 if no skipping */
		   unsigned int n, /* number of data (x,y,z) weighted by w */
		   double *x,	     
		   double *y,
		   double *z,
		   double *w,
		   double *z_last, /* last estimate of z at (x,y) */
		   double xr,	   /* influence radius (xr, yr) */
		   double yr);


SEXP interp_barnes(SEXP x, SEXP y, SEXP z, SEXP w, /* z at (x,y), weighted by w */
		  SEXP xg, SEXP yg, /* grid */
		  SEXP xr, SEXP yr, /* influence radii */
		  SEXP gamma,	    /* radius-reduction factor, squared */
		  SEXP iterations   /* number of iterations */
	)
{
	double *rx, *ry, *rz, *rw; /* data */
	double *rxg, *ryg;	   /* grid */
	double *rxr, *ryr;	   /* radii */
	double *rgamma;		   /* gamma */
	int *niter;		   /* num iterations */
	int nx, ny, nz, nxg, nyg, i, j, k, it;
	double *z_last, *z_last2; /* previous values */
	double *rans;		  /* result */
	double xr2, yr2;	  /* radii, adjusting */
	SEXP ans;
	
	nx = length(x); ny = length(y);	nz = length(z);
	if (nx != ny) error("lengths of x and y must agree, but they are %d and %d", nx, ny);
	if (nx != nz) error("lengths of x and z must agree, but they are %d and %d", nx, nz);
	rx = REAL(x); ry = REAL(y); rz = REAL(z); rw = REAL(w);
	
	nxg = length(xg);
	nyg = length(yg);
	rxg = REAL(xg);
	ryg = REAL(yg);
	rgamma = REAL(gamma);
	niter = INTEGER(iterations);
	rxr = REAL(xr);
	ryr = REAL(yr);
	xr2 = *rxr;
	yr2 = *ryr;
	
	/* previous values */
	z_last = (double*)malloc(nx * sizeof(double));
	z_last2 = (double*)malloc(nx * sizeof(double));

	for (i = 0; i < nxg; i++) for (j = 0; j < nyg; j++) rans[i + nxg*j] = 0.0;

	for (k = 0; k < nx; k++)
		z_last[k] = z_last2[k] = 0.0;

	PROTECT(ans = allocMatrix(REALSXP, nxg, nyg));
	rans = REAL(ans);
	for (it = 0; it < *niter; it++) {
		/* update grid */
		for (i = 0; i < nxg; i++) {
			for (j = 0; j < nyg; j++) {
				rans[i + nxg*j] = interpolate_barnes(rxg[i],
								     ryg[j],
								     rans[i + nxg*j],
								     -1, // no skip
								     nx,
								     rx,
								     ry,
								     rz,
								     rw,
								     z_last,
								     xr2,
								     yr2);
			}
		}
		/* interpolate grid back to data locations */
		for (k = 0; k < nx; k++) {
			z_last2[k] = interpolate_barnes(rx[k],
							ry[k],
							z_last[k],
							-1, /* BUG: why not skip? */
							nx,
							rx,
							ry,
							rz,
							rw,
							z_last,
							xr2,
							yr2);
		}
		for (k = 0; k < nx; k++)
			z_last[k] = z_last2[k];
		if (*rgamma > 0.0) {
			/* refine search range for next iteration */
			xr2 *= sqrt(*rgamma);
			yr2 *= sqrt(*rgamma);
		}
	}
	free(z_last);
	free(z_last2);
	UNPROTECT(1);
	return(ans);
}

/* Do interpolation search, using bisection rule on possibly irregular
 * array g[].
 *
 * If 'x' is in the range of the grid, defined by g[0] to g[ng-1],
 * then set 'b' and 'f' such that
 *     x = g[b] + f * (g[b+1] - g[b])
 * and return 1.
 *
 * If 'x' is not in the range, set b to the nearest endpoint, 
 * set f to the distance to the nearest endpoint and return 0.
 */
static int
nearest(double x, double g[], unsigned int ng, int *b, double *f)
{
	int l = 0;		/* left index */
	int r = ng - 1;		/* right index */
	int m;			/* middle index */
	if (g[0] < g[1]) {	/* ascending sequence */
		if (x <= g[l])	{ *b = 0; *f = g[l] - x; return 0; }
		if (g[r] <= x)	{ *b = r; *f = x - g[r]; return 0; }
		m = (l + r) / 2;
		while (r - l > 1) {
			if (x < g[m])
				r = m;
			else if (g[m] < x)
				l = m;
			else { 
				*b = m;
				*f = (x - g[*b]) / (g[*b+1] - g[*b]);
				return 1;
			}
			m = (r + l) / 2;
		}
		*b = l;
		*f = (x - g[*b]) / (g[*b+1] - g[*b]);
		return 1;
	} else {			/* descending sequence */
		if (x >= g[l])	{ *b = 0; *f = g[l] - x; return 0; }
		if (g[r] >= x)	{ *b = r; *f = x - g[r]; return 0; }
		m = (l + r) / 2;
		while (r - l > 1) {
			if (x > g[m])
				r = m;
			else if (g[m] > x)
				l = m;
			else {
				*b = m;
				*f = (x - g[*b]) / (g[*b+1] - g[*b]);
				return 1;
			}
			m = (r + l) / 2;
		}
		*b =  l;
		*f = (x - g[*b]) / (g[*b+1] - g[*b]);
		return 1;
	}
}



static double
interpolate_barnes(double xx, /* interpolate to get value at (xx,yy) */
		   double yy,	/* given previous value zz there */
		   double zz,	
		   int skip, /* value in (x,y,z) to skip, or -1 if no skipping */
		   unsigned int n, /* number of data (x,y,z) weighted by w */
		   double *x,	     
		   double *y,
		   double *z,
		   double *w,
		   double *z_last, /* last estimate of z at (x,y) */
		   double xr,	   /* influence radius (xr, yr) */
		   double yr)
{
	double sum = 0.0, sum_w = 0.0;
	double dx, dy, d;
	int k;
	for (k = 0; k < n; k++) {
		double weight;
		if (k != skip) {
			dx = (xx - x[k]) / xr;
			dy = (yy - y[k]) / yr;
			d = dx*dx + dy*dy;
#ifdef USE_APPROX_EXP
			weight = w[k] / (0.999448 +
					 d * (1.023820 +
					      d * (0.3613967 +
						   d * (0.4169646 +
							d * (-0.1292509 +
							     d * 0.0499565)))));
#else
			weight = w[k] * exp(-d);
#endif
			sum += weight * (z[k] - z_last[k]);
			sum_w += weight;
		}
	}
	if (sum_w > 0.0)
		return (zz + sum / sum_w);
	else
		return NA_REAL;
}
