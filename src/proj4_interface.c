#include <stdio.h>
#include <string.h>
#include <R.h>
#include <proj_api.h>

//#define DEBUG 1

/* NOTES.
 * 1. must provide an ellipse model.  The proj4 package defaults this to "sphere".
 */

/* TEST CODE.
 
   library(oce)
   lon <- c(0, 1)
   lat <- c(1, 0)
   proj <- "+proj=merc +ellps=WGS84"
   n <- length(lon)
   xy <- .C("proj4_interface", as.character(proj), as.integer(TRUE), as.integer(n), as.double(lon), as.double(lat), X=double(n), Y=double(n))
   ll <- .C("proj4_interface", as.character(proj), as.integer(FALSE), as.integer(n), as.double(xy$x), as.double(xy$y), X=double(n), Y=double(n))

   data.frame(x=xy$X, y=xy$Y, lonMismatch=lon-ll$X, LatMismatch=lat-ll$Y)

   data(coastlineWorldFine, package="ocedata")
   lon <- coastlineWorldFine[['longitude']]
   lat <- coastlineWorldFine[['latitude']]
   n <- length(lon)
   proj <- "+proj=moll +ellps=sphere" # mercator is ugly on world views
   system.time(xy<-.C("proj4_interface", as.character(proj), as.integer(TRUE), as.integer(n), as.double(lon), as.double(lat), X=double(n), Y=double(n), NAOK=TRUE))
   par(mar=c(2, 2, 1, 1), mgp=c(2, 0.7, 0))
   plot(xy$X, xy$Y, type='l', asp=1, xlab="", ylab="")

*/

/* TEST CODE RESULTS.
 *
 *           x            y  lonMismatch LatMismatch
 *  1      0.0 1.105800e+05 4.58647e-255           1
 *  2 111319.5 7.081155e-10  1.00000e+00           0
 *
 *  user  system elapsed 
 * 0.113   0.004   0.116 
 */


void proj4_interface(char **proj_spec, int *forward, int *n, double *x, double *y, double *X, double *Y)
{
    // project (x,y) -> (X, Y), forward or inverse; *n is length of *x.
    projPJ pj;
    projUV xy, XY;
#ifdef DEBUG
    Rprintf("mapping=%s, proj='%s'\n", *forward?"forward":"inverse", *proj_spec);
#endif
    if (!(pj = pj_init_plus(*proj_spec))) 
        error("ERROR %s\n", pj_strerrno(*pj_get_errno_ref()));
    if (*forward != 0 && *forward != 1)
        error("forward must be 0 or 1");
    double dpr = 180.0 / M_PI;  // degrees per radian
#ifdef DEBUG
    Rprintf(" *n = %d\n", *n);
    Rprintf(" x[0] = %g\n", x[0]);
    Rprintf(" y[0] = %g\n", y[0]);
#endif
    for (int i = 0; i < (*n); i++) {
        if (ISNA(x[i]) || ISNA(y[i])) {
            X[i] = NA_REAL;
            Y[i] = NA_REAL;
        } else {
            if (*forward) {
                xy.u = x[i] / dpr;
                xy.v = y[i] / dpr;
                XY = pj_fwd(xy, pj);
                X[i] = XY.u;
                Y[i] = XY.v;
#ifdef DEBUG
                Rprintf("i = %d, x = %f, y = %f, X = %f, Y = %f\n", i, x[i], y[i], X[i], Y[i]);
#endif
            } else {
                xy.u = x[i];
                xy.v = y[i];
#ifdef DEBUG
                Rprintf("x[%d]=%g y[%d]=%g ... about to do inverse\n", i, xy.u, i, xy.v);
#endif
                XY = pj_inv(xy, pj);
                X[i] = XY.u * dpr;
                Y[i] = XY.v * dpr;
#ifdef DEBUG
                Rprintf("i = %d, x = %f, y = %f, X = %f, Y = %f\n", i, x[i], y[i], X[i], Y[i]);
#endif
            }
        }
    }
    pj_free(pj);
}

