// assemble_polygons(): construct lat-lon polygons, hopefully to speed up mapImage()
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>

//#define DEBUG

/*

   system("R CMD SHLIB map.c") 
   dyn.load("map.so")
   D <- .Call("assemble_polygons", c(0, 1, 2), c(10, 11, 12))
   plot(D$longitude, D$latitude)
   polygon(D$longitude, D$latitude, col=rainbow(4))


   library(oce)
   library(ncdf)
   con <- open.ncdf("/data/oar/levitus/temperature_annual_1deg.nc")
   lon <- get.var.ncdf(con, "lon")
   lat <- get.var.ncdf(con, "lat")
   SST <- get.var.ncdf(con, "t_an")[,,1]
   Tlim <- c(-2, 30)


*/

// macro to index an array
#define ij(i, j) ((i) + (nrow) * (j))

SEXP map_assemble_polygons(SEXP lon, SEXP lat, SEXP z)
{
    PROTECT(lon = AS_NUMERIC(lon));
    double *lonp = REAL(lon);
    PROTECT(lat = AS_NUMERIC(lat));
    double *latp = REAL(lat);
    PROTECT(z = AS_NUMERIC(z));
    double *zp = REAL(z);
    int nlat = length(lat);
    int nlon = length(lon);
    if (nlon < 1) error("must have at least 2 longitudes");
    if (nlat < 1) error("must have at least 2 latitudes");

    // Note that first dimension of z is for y (here, lat) and second for x (here, lon)

    int nrow = INTEGER(GET_DIM(z))[0];
    int ncol = INTEGER(GET_DIM(z))[1];
    if (nlat != ncol) error("mismatch; length(lat)=%d must equal nrow(z)=%d", nlat, ncol);
    if (nlon != nrow) error("mismatch; length(lon)=%d must equal ncol(z)=%d", nlon, nrow);

    int n = nlon * nlat;
    SEXP polylon, polylat, polyz; 
    PROTECT(polylon = allocVector(REALSXP, 5*n));
    PROTECT(polylat = allocVector(REALSXP, 5*n));
    PROTECT(polyz = allocMatrix(REALSXP, nlon, nlat));
    double *polylonp = REAL(polylon), *polylatp = REAL(polylat), *polyzp = REAL(polyz);

    double latstep = 0.5 * fabs(latp[1] - latp[0]);
    double lonstep = 0.5 * fabs(lonp[1] - lonp[0]);
#ifdef DEBUG
    Rprintf("nlon: %d, nlat: %d, latstep: %f, lonstep: %f\n", nlon, nlat, latstep, lonstep);
#endif
    int k = 0, l=0; // indices for points and polygons
    for (int j = 0; j < ncol; j++) {
        for (int i = 0; i < nrow; i++) {
#ifdef DEBUG
            if (j == 0 && i < 3)
                Rprintf("i: %d, j: %d, lon: %.4f, lat:%.4f, k: %d\n", i, j, lonp[i], latp[j], k);
#endif
            // Lower left
            polylonp[k] = lonp[i] - lonstep;
            polylatp[k++] = latp[j] - latstep;
            // Upper left
            polylonp[k] = lonp[i] - lonstep;
            polylatp[k++] = latp[j] + latstep;
            // Upper right
            polylonp[k] = lonp[i] + lonstep;
            polylatp[k++] = latp[j] + latstep;
            // Lower right
            polylonp[k] = lonp[i] + lonstep;
            polylatp[k++] = latp[j] - latstep;
            // end
            polylonp[k] = NA_REAL;
            polylatp[k++] = NA_REAL;
            polyzp[l++] = zp[ij(i, j)];
#ifdef DEBUG
            if (j == 0 && i < 3)
                for (int kk=k-5; kk<k-1; kk++)
                    Rprintf("k: %d, lon: %.4f, lat:%.4f\n", kk, polylonp[kk], polylatp[kk]);
#endif
        }
        if (k > 5 * n)
            error("coding error (assigned insufficient memory); k: %d,  5*n: %d", k, 5*n);
    }
    if (k != 5 * n)
        error("coding error (assigned surplus memory); k: %d,  5*n: %d", k, 5*n);
    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 3));
    PROTECT(res_names = allocVector(STRSXP, 3));
    SET_VECTOR_ELT(res, 0, polylon);
    SET_STRING_ELT(res_names, 0, mkChar("longitude"));
    SET_VECTOR_ELT(res, 1, polylat);
    SET_STRING_ELT(res_names, 1, mkChar("latitude"));
    SET_VECTOR_ELT(res, 2, polyz);
    SET_STRING_ELT(res_names, 2, mkChar("z"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(8);
    return(res);
}


SEXP map_check_polygons(SEXP x, SEXP y, SEXP z, SEXP xokspan, SEXP usr) // returns new x vector
{
    //int nrow = INTEGER(GET_DIM(z))[0];
    //int ncol = INTEGER(GET_DIM(z))[1];
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(z = AS_NUMERIC(z));
    PROTECT(xokspan = AS_NUMERIC(xokspan));
    PROTECT(usr = AS_NUMERIC(usr));
    int nusr = LENGTH(usr);
    if (nusr != 4) error("'usr' must hold 4 values");
    double *usrp = REAL(usr); // left right bottom top
    double *xp = REAL(x);
    double *yp = REAL(y);
    //double *zp = REAL(z);
    double *xokspanp = REAL(xokspan);
    int nx = length(x);
    int ny = length(y);
    int nz = length(z);
    if (nx < 2) error("must have at least two x values");
    if (ny < 2) error("must have at least two y values");
    if (nz < 1) error("must have at least one z value");
    int npoly = nx / 5;

    SEXP okPoint, okPolygon, clippedPoint, clippedPolygon;
    PROTECT(okPolygon = allocVector(LGLSXP, npoly)); 
    PROTECT(okPoint = allocVector(LGLSXP, nx)); 
    PROTECT(clippedPoint = allocVector(LGLSXP, nx)); 
    PROTECT(clippedPolygon = allocVector(LGLSXP, npoly)); 

    int *okPointp = INTEGER(okPoint);
    int *okPolygonp = INTEGER(okPolygon);
    int *clippedPointp = INTEGER(clippedPoint);
    int *clippedPolygonp = INTEGER(clippedPolygon);
    // Initialize (not be needed if below catches all cases)
    for (int ipoly = 0; ipoly < npoly; ipoly++) {
        okPolygonp[ipoly] = 1;
        clippedPolygonp[ipoly] = 0;
    }
    for (int ix = 0; ix < nx; ix++) {
        okPointp[ix] = 1;
        clippedPointp[ix] = 0;
    }
    // x1 x2 x3 x4 NA x1 x2 x3 x4 NA ...
    double dxPermitted = fabs(*xokspanp);
#ifdef DEBUG
    int count = 0, ncount=100000;
#endif
    for (int ipoly = 0; ipoly < npoly; ipoly++) {
        int start = 5 * ipoly;
        // Check for bad polygons, in three phases.
        // 1. Find polygons that have some NA values for vertices
#ifdef DEBUG
        if (ipoly < 3)
            Rprintf("start: %d; okPointp= %d %d ...\n", start, okPointp[start], okPointp[start+1]);
#endif
        for (int j = 0; j < 4; j++) { // skip 5th point which is surely NA
            // Check for x or y being NA
            if (ISNA(xp[start + j]) || ISNA(yp[start + j])) {
#ifdef DEBUG
                if (count++ < ncount) { // FIXME: remove when working
                    Rprintf("(1.) x or y is NA -- ipoly: %d, j: %d, span: %f (limit to span: %f)\n",
                            ipoly, j, fabs(xp[start+j]-xp[start+j-1]), dxPermitted);
                }
#endif
                for (int k = 0; k < 5; k++)
                    okPointp[start + k] = 0;
                okPolygonp[ipoly] = 0;
                break;
            }
        }
        // 2. Find polygons with all vertices outside the plot region
        double xmin = xp[start], xmax = xp[start], ymin = yp[start], ymax=yp[start];
        for (int j = 1; j < 4; j++) {
            if (xp[start + j] < xmin) xmin = xp[start + j];
            if (yp[start + j] < ymin) ymin = yp[start + j];
            if (xp[start + j] > xmax) xmax = xp[start + j];
            if (yp[start + j] > ymax) ymax = yp[start + j];
        }
        if (xmax < usrp[0] || usrp[1] < xmin || ymax < usrp[2] || usrp[3] < ymin) {
#ifdef DEBUG
            if (count < ncount) {
                count++;
                Rprintf("clipping points %d to %d\n", start, start+4);
            }
#endif
            for (int k = 0; k < 5; k++) {
                clippedPointp[start + k] = 1;
            }
            clippedPolygonp[ipoly] = 1;
        }
        // 3. Find polygons with excessive x range (an error in projection)
        for (int j = 1; j < 4; j++) { // skip 5th point which is surely NA
            if (dxPermitted < fabs(xp[start + j] - xp[start + j - 1])) {
#ifdef DEBUG
                if (count++ < ncount) { // FIXME: remove when working
                    Rprintf("(3.) ipoly: %d, j: %d, span: %f (limit to span: %f)\n",
                            ipoly, j, fabs(xp[start+j]-xp[start+j-1]), dxPermitted);
                }
#endif
                for (int k = 0; k < 5; k++) {
                    okPointp[start + k] = 0;
                }
                okPolygonp[ipoly] = 0;
                break;
            }
        }
    }
    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 4));
    PROTECT(res_names = allocVector(STRSXP, 4));
    SET_VECTOR_ELT(res, 0, okPoint);
    SET_STRING_ELT(res_names, 0, mkChar("okPoint"));
    SET_VECTOR_ELT(res, 1, clippedPoint);
    SET_STRING_ELT(res_names, 1, mkChar("clippedPoint"));
    SET_VECTOR_ELT(res, 2, okPolygon);
    SET_STRING_ELT(res_names, 2, mkChar("okPolygon"));
    SET_VECTOR_ELT(res, 3, clippedPolygon);
    SET_STRING_ELT(res_names, 3, mkChar("clippedPolygon"));
    setAttrib(res, R_NamesSymbol, res_names);

    UNPROTECT(11);
    return(res);
#undef ij
}

#define INCREMENT_J                                                               \
    if (j > (clen - 2)) {                                                         \
        /*Rprintf("INCREASE storage from %d to %d [a]\n", clen, (int)(100 + clen));*/ \
        clen += 100;                                                              \
        xbp = (double*)Realloc(xbp, clen, double);                                \
        ybp = (double*)Realloc(ybp, clen, double);                                \
    }                                                                             \
    j++;


// Eliminate any coastline segments that lie wholly outside par("usr")
SEXP map_clip_xy_OLD_BROKEN(SEXP x, SEXP y, SEXP usr) // returns list with new x and y vectors
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(usr = AS_NUMERIC(usr));
    int nusr = LENGTH(usr);
    if (nusr != 4)
        error("'usr' must hold 4 values, not ", nusr);
    double *usrp = REAL(usr); // left right bottom top
    double *xp = REAL(x);
    double *yp = REAL(y);
    int xlen = length(x);
    int ylen = length(y);
    if (xlen != ylen)
        error("'x' and 'y' must be of same length");
    if (xlen < 2)
        error("must have at least two 'x' and 'y' pairs");
    // xbp and xbp are growable buffers
    int clen = xlen + 100; // the 100 may save reallocs
    double *xbp = (double*)Calloc((size_t)clen, double);
    double *ybp = (double*)Calloc((size_t)clen, double);
#ifdef DEBUG
    double distMIN = 10e6; // FIXME: temporary to find problem in Greenland
#endif
    // Find chunks, and copy any with 1 or more datum in the usr window.
    int i, j = 0;
#ifdef DEBUG
    Rprintf("usrp=%.0f %.0f %.0f %.0f\n", usrp[0], usrp[1], usrp[2], usrp[3]);
    double danSmall = 1e10;
#endif
    for (i = 0; i < xlen; i++) { 
        if (ISNA(xp[i])) {
            if (9428-5 <= i && i <= 9865+5)
                Rprintf("NA at i=%d (pinned between 9428 and 9865)\n", i);
            // i points to the NA at the end of a sequence; only emit one NA
            // even if there might be multiples
#ifdef DEBUG
            Rprintf("TOP NA i=%d j=%d; xlen=%d\n", i, j, xlen);
#endif
            if (j == 0 || !ISNA(xbp[j-1])) {
                xbp[j] = NA_REAL;
                ybp[j] = NA_REAL;
                INCREMENT_J;
            }
        } else {
            if (usrp[0] <= xp[i] && xp[i] <= usrp[1] && usrp[2] <= yp[i] && yp[i] <= usrp[3]) {
                for (int ii = i; ii < xlen; ii++) {
                    if (ISNAN(xp[ii])) {
#ifdef DEBUG
                        Rprintf("NA (end of trace) i=%d ii=%d j=%d\n", i, ii, j);
#endif
                        if (ii > 0 && xp[ii-1] != xp[i] && yp[ii-1] != yp[i]) { // FIXME: close polygons
#ifdef DEBUG
                            double dan = sqrt((xp[i]-4081795)*(xp[i]-4081795) + (yp[i]-9461998)*(yp[i]-9461998))/1e3;
                            Rprintf("COMPLETING broken polygon at i=%d ii=%d j=%d, repeating xp=%.0f yp=%.0f; dan=%.0f, danSmall=%.0f\n", i,ii,j,xp[i],yp[i],dan,danSmall);
                            if (dan < danSmall) {
                                Rprintf("  ^^^^^^^\n");
                                danSmall = dan; // 10311
                            }
#endif
                            xbp[j] = xp[i];
                            ybp[j] = yp[i];
                            INCREMENT_J;
                        }
                        xbp[j] = NA_REAL;
                        ybp[j] = NA_REAL;
                        INCREMENT_J;
                        i = ii; // FIXME: or -1 or +1?
                        break;
                    } else {
#ifdef DEBUG
                        double dx = xp[ii] - (-1.0e6);
                        double dy = yp[ii] - (-0.5e6);
                        double dist = sqrt(dx*dx + dy*dy) / 1000.0;
                        Rprintf("INSIDE i=%d ii=%d j=%d xp[i]=%.0f yp[i]=%.0f\n", i, ii, j, xp[i], yp[i]);
                        if (dist < distMIN) {
                            Rprintf("CLOSEST\n");
                            distMIN = dist;
                        }
#endif
                        xbp[j] = xp[ii];
                        ybp[j] = yp[ii];
                        INCREMENT_J;
                    }
                }
            } else {
#ifdef DEBUG
                Rprintf("OUTSIDE i=%d j=%d xp[i]=%.0f yp[i]=%.0f\n", i, j, xp[i], yp[i]);
#endif
            }
        }
    }
    SEXP xc;
    PROTECT(xc = NEW_NUMERIC(j));
    double *xcp = REAL(xc);
    SEXP yc;
    PROTECT(yc = NEW_NUMERIC(j));
    double *ycp = REAL(yc);
    for (int jj = 0; jj < j; jj++) {
        xcp[jj] = xbp[jj];
        ycp[jj] = ybp[jj];
    }

    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 2));
    PROTECT(res_names = allocVector(STRSXP, 2));
    SET_VECTOR_ELT(res, 0, xc);
    SET_STRING_ELT(res_names, 0, mkChar("x"));
    SET_VECTOR_ELT(res, 1, yc);
    SET_STRING_ELT(res_names, 1, mkChar("y"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(7);
    return(res);
}




// Eliminate any coastline segments that lie wholly outside par("usr")
SEXP map_clip_xy(SEXP x, SEXP y, SEXP usr) // returns list with new x and y vectors
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(usr = AS_NUMERIC(usr));
    int nusr = LENGTH(usr);
    if (nusr != 4)
        error("'usr' must hold 4 values, not ", nusr);
    double *usrp = REAL(usr); // left right bottom top
    double *xp = REAL(x);
    double *yp = REAL(y);
    int xlen = length(x);
    int ylen = length(y);
    if (xlen != ylen)
        error("'x' and 'y' must be of same length");
    if (xlen < 2)
        error("must have at least two 'x' and 'y' pairs");
    // xb and yb are growable buffers; we copy to xc and yc near the end.
    int clen = xlen + 100; // the 100 may save reallocs
    double *xbp = (double*)Calloc((size_t)clen, double);
    double *ybp = (double*)Calloc((size_t)clen, double);
#ifdef DEBUG
    double distMIN = 10e6; // FIXME: temporary to find problem in Greenland
#endif
    // Find chunks, and copy any with 1 or more datum in the usr window.
#ifdef DEBUG
    Rprintf("usrp=%.0f %.0f %.0f %.0f\n", usrp[0], usrp[1], usrp[2], usrp[3]);
    double danSmall = 1e10;
#endif
    // flush any NA at the start
    int i = 0;
    while (i < xlen && ISNA(xp[i]))
        i++;
    int istart = i, iend = -1;
    int j = 0;
    for (; i < xlen; i++) {
        if (ISNA(xp[i])) {
            iend = i - 1;
            while (i < xlen && ISNA(xp[i]))
                i++;
            // Save the polygon, if it intersects par("usr")
            for (int ii = istart; ii <= iend; ii++) {
                if (usrp[0] <= xp[ii] && xp[ii] <= usrp[1] && usrp[2] <= yp[ii] && yp[ii] <= usrp[3]) {
#ifdef DEBUG
                   Rprintf("istart=%d iend=%d intersects par('usr')\n", istart, iend);
#endif
                   for (int k = istart; k <= iend; k++) {
                       xbp[j] = xp[k];
                       ybp[j] = yp[k];
                       INCREMENT_J;
                   }
#ifdef DEBUG
                   Rprintf("  copied to output buffer, so now j=%d\n", j);
#endif
                   break;
                }
            }
            istart = i;
            xbp[j] = NA_REAL;
            ybp[j] = NA_REAL;
            INCREMENT_J;
        }
    }
    SEXP xc;
    PROTECT(xc = NEW_NUMERIC(j));
    double *xcp = REAL(xc);
    SEXP yc;
    PROTECT(yc = NEW_NUMERIC(j));
    double *ycp = REAL(yc);
    for (int jj = 0; jj < j; jj++) {
        xcp[jj] = xbp[jj]; // FIXME: just copying for now
        ycp[jj] = ybp[jj];
    }

    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 2));
    PROTECT(res_names = allocVector(STRSXP, 2));
    SET_VECTOR_ELT(res, 0, xc);
    SET_STRING_ELT(res_names, 0, mkChar("x"));
    SET_VECTOR_ELT(res, 1, yc);
    SET_STRING_ELT(res_names, 1, mkChar("y"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(7);
    return(res);
}


