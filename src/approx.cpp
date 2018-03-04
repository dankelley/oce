#include <Rcpp.h>
using namespace Rcpp;

// This is a utility function, so its wrapper is not put in the R namespace,
// which is why there is no roxygen @export here, and no roxygen documentation,
// either. However, we need to tell Rcpp to export it, of course.
//
// NOTE: update src/registerDynamicSymbol.c for this!
//
// [[Rcpp::export]]
NumericVector do_approx3d(NumericVector x, NumericVector y, NumericVector z,
        NumericVector f, // actually it's a 3D array
        NumericVector xout, NumericVector yout, NumericVector zout)
{
#define ijk(i, j, k) ((i) + (nx) * (j) + (nx) * (ny) * (k))
    int nx = x.size();
    int ny = y.size();
    int nz = z.size();
#ifdef debug
    Rprintf("nx=%d ny=%d nz=%d\n", nx, ny, nz);
#endif
    int nout = xout.size();
    NumericVector res(nout);
    double dx = x[1] - x[0];
    double dy = y[1] - y[0];
    double dz = z[1] - z[0];
#ifdef debug
    Rprintf("dx=%f dy=%f dz=%f\n", dx, dy, dz);
#endif
#ifdef debug2
    for (int iz = 0; iz < nz; iz++) {
        for (int iy = 0; iy < ny; iy++) {
            for (int ix = 0; ix < nx; ix++) {
                int ii = ijk(ix, iy, iz);
                Rprintf("fg[%d, %d, %d (AKA %3d)] = %10f (indices in R notation)\n",
                        ix+1, iy+1, iz+1, ii+1, f[ii]);
            }
        }
    }
#endif
    for (int i = 0; i < nout; i++) {
        int ix = (int)floor((xout[i] - x[0]) / dx);
        int iy = (int)floor((yout[i] - y[0]) / dy);
        int iz = (int)floor((zout[i] - z[0]) / dz);
        // FIXME: could add arg 'rule' like in approx(), but what if offbounds in more than 1 direction?
        if (ix < 0 || ix >= (nx-1) || iy < 0 || iy >= (ny-1) || iz < 0 || iz >= (nz-1)) {
#ifdef debug
            Rprintf("  point %d at xyz=(%f %f %f) maps to indices [%d %d %d] in R notation, which is outside the array bounds;      result is NA\n",
                    i, xout[i], yout[i], zout[i], 1+ix, 1+iy, 1+iz);
#endif
            res[i] = NA_REAL;
        } else {
            // Now, for the guts of the interpolation.  Note the construction of the formula,
            // with terms proportional to xx or (1-xx), etc., which reveals the roots
            // The names for the "f..." tems is by location e.g. 010, means x=min_bin_x,
            // y=max_bin_y and z=min_bin_z.
            double f000 = f[ijk(ix  , iy  , iz  )];
            double f001 = f[ijk(ix  , iy  , iz+1)];
            double f010 = f[ijk(ix  , iy+1, iz  )];
            double f011 = f[ijk(ix  , iy+1, iz+1)];
            double f100 = f[ijk(ix+1, iy  , iz  )];
            double f101 = f[ijk(ix+1, iy  , iz+1)];
            double f110 = f[ijk(ix+1, iy+1, iz  )];
            double f111 = f[ijk(ix+1, iy+1, iz+1)];
            double xx = (xout[i] - x[ix]) / dx;
            double yy = (yout[i] - y[iy]) / dy;
            double zz = (zout[i] - z[iz]) / dz;
            double ff  = f000 * (1 - xx) * (1 - yy) * (1 - zz) +
                f100 * xx * (1 - yy) * (1 - zz) +
                f010 * (1 - xx) * yy * (1 - zz) +
                f001 * (1 - xx) * (1 - yy) * zz +
                f101 * xx * (1 - yy) * zz +
                f011 * (1 - xx) * yy * zz +
                f110 * xx * yy * (1 - zz) +
                f111 * xx * yy * zz;
            res[i] = ff;
#ifdef debug
            Rprintf("  point %d at xyz=(%f %f %f) maps to indices [%d %d %d] in R notation, where xyz=(%f %f %f); result is %f\n",
                    i, xout[i], yout[i], zout[i], 1+ix, 1+iy, 1+iz, x[ix], y[iy], z[iz], res[i]);
#endif
        }
    }
    return(res);
}

