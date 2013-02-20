#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define debug
//#define debug2

#define ijk(i, j, k) ((i) + (nx) * (j) + (nx) * (ny) * (k))

SEXP approx3d(SEXP x, SEXP y, SEXP z, SEXP f, SEXP xout, SEXP yout, SEXP zout)
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(z = AS_NUMERIC(z));
    PROTECT(f = AS_NUMERIC(f));
    PROTECT(xout = AS_NUMERIC(xout));
    PROTECT(yout = AS_NUMERIC(yout));
    PROTECT(zout = AS_NUMERIC(zout));
    int nx = LENGTH(x);
    int ny = LENGTH(y);
    int nz = LENGTH(z);
    int *dim = INTEGER(GET_DIM(f));
    int rank = LENGTH(GET_DIM(f));
    if (3 != rank)
        error("grid must be 3D but it is %dD\n", rank);
#ifdef debug
    Rprintf("nx=%d ny=%d nz=%d\n", nx, ny, nz);
    Rprintf("DIM=%d x %d x %d\n", dim[0], dim[1], dim[2]);
#endif
    if (nx != dim[0])
        error("grid mismatch; length(x) is %d but first dimension of grid is %d\n", nx, dim[0]);
    if (ny != dim[1])
        error("grid mismatch; length(y) is %d but first dimension of grid is %d\n", ny, dim[1]);
    if (nz != dim[2])
        error("grid mismatch; length(z) is %d but first dimension of grid is %d\n", nz, dim[2]);
    int nxout = LENGTH(xout);
    int nyout = LENGTH(yout);
    if (nxout != nyout)
        error("grid mismatch; length(x) must match length(y) but they are %d and %d\n", nxout, nyout);
    int nzout = LENGTH(zout);
    if (nxout != nzout)
        error("grid mismatch; length(x) must match length(z) but they are %d and %d\n", nxout, nzout);
    // FIXME: figure out how array is done (i.e. which index goes first?)
    SEXP res;
    PROTECT(res = allocVector(REALSXP, nxout));
    double *xp = REAL(x);
    double *yp = REAL(y);
    double *zp = REAL(z);
    double *fp = REAL(f);
    double *xoutp = REAL(xout);
    double *youtp = REAL(yout);
    double *zoutp = REAL(zout);
    double *resp = REAL(res);
    double dx = xp[1] - xp[0]; // FIXME: R code should check that grid is regular
    double dy = yp[1] - yp[0];
    double dz = zp[1] - zp[0];
#ifdef debug
    Rprintf("dx=%f dy=%f dz=%f\n", dx, dy, dz);
#endif
    int N = nx * ny * nz;
#ifdef debug2
    for (int iz = 0; iz < nz; iz++) {
        for (int iy = 0; iy < ny; iy++) {
            for (int ix = 0; ix < nx; ix++) {
                int ii = ijk(ix, iy, iz);
                Rprintf("fg[%d, %d, %d (AKA %3d)] = %10f (indices in R notation)\n",
                        ix+1, iy+1, iz+1, ii+1, fp[ii]);
            }
        }
    }
#endif
    for (int i = 0; i < nxout; i++) {
        int ix = (int)floor((xoutp[i] - xp[0]) / dx);
        int iy = (int)floor((youtp[i] - yp[0]) / dy);
        int iz = (int)floor((zoutp[i] - zp[0]) / dz);
        // FIXME: could add arg 'rule' like in approx(), but what if offbounds in more than 1 direction?
        if (ix < 0 || ix >= (nx-1) || iy < 0 || iy >= (ny-1) || iz < 0 || iz >= (nz-1)) {
#ifdef debug
            Rprintf("  point %d at xyz=(%f %f %f) maps to indices [%d %d %d] in R notation, which is outside the array bounds;      result is NA\n",
                    i, xoutp[i], youtp[i], zoutp[i], 1+ix, 1+iy, 1+iz);
#endif
            resp[i] = NA_REAL;
        } else {
            // Now, for the guts of the interpolation.  Note the construction of the formula,
            // with terms proportional to xx or (1-xx), etc., which reveals the roots
            // The names for the "f..." tems is by location e.g. 010, means x=min_bin_x,
            // y=max_bin_y and z=min_bin_z.
            double f000 = fp[ijk(ix  , iy  , iz  )];
            double f001 = fp[ijk(ix  , iy  , iz+1)];
            double f010 = fp[ijk(ix  , iy+1, iz  )];
            double f011 = fp[ijk(ix  , iy+1, iz+1)];
            double f100 = fp[ijk(ix+1, iy  , iz  )];
            double f101 = fp[ijk(ix+1, iy  , iz+1)];
            double f110 = fp[ijk(ix+1, iy+1, iz  )];
            double f111 = fp[ijk(ix+1, iy+1, iz+1)];
            double xx = (xoutp[i] - xp[ix]) / dx;
            double yy = (youtp[i] - yp[iy]) / dy;
            double zz = (zoutp[i] - zp[iz]) / dz;
            double ff  = f000 * (1 - xx) * (1 - yy) * (1 - zz) +
                f100 * xx * (1 - yy) * (1 - zz) + 
                f010 * (1 - xx) * yy * (1 - zz) + 
                f001 * (1 - xx) * (1 - yy) * zz +
                f101 * xx * (1 - yy) * zz + 
                f011 * (1 - xx) * yy * zz + 
                f110 * xx * yy * (1 - zz) + 
                f111 * xx * yy * zz;
            resp[i] = ff;
#ifdef debug
            Rprintf("  point %d at xyz=(%f %f %f) maps to indices [%d %d %d] in R notation, where xyz=(%f %f %f); result is %f\n",
                    i, xoutp[i], youtp[i], zoutp[i], 1+ix, 1+iy, 1+iz, xp[ix], yp[iy], zp[iz], resp[i]);
#endif
        }
    }
    UNPROTECT(8);
    return(res);
}

