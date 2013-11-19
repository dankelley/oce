// assemble_polygons(): construct lat-lon polygons, hopefully to speed up mapImage()
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>

#define DEBUG

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

system("R CMD SHLIB map.c") 
dyn.load("map.so")
poly <- .Call("map_assemble_polygons", lon, lat)
drawPalette(Tlim, col=oceColorsJet)
mapPlot(coastlineWorld, projection='mollweide', grid=FALSE)
xy <- mapproject(poly$longitude, poly$latitude)
pal <- oceColorsJet(100)
plot(range(xy$x, na.rm=TRUE), range(xy$y, na.rm=TRUE), type='n', asp=1, xlab="", ylab="", axes=FALSE)
ok <- .Call("map_repair_polygons", xy$x, xy$y, diff(par('usr'))[1:2]/5)
i<-20702+seq(-10,10); data.frame(i=i,ok=ok[i],x=xy$x[i],y=xy$y[i])
polygon(ok, xy$y, col=pal[rescale(as.vector(SST),Tlim[1],Tlim[2],1,100)],border=NA)



*/

SEXP map_assemble_polygons(SEXP lon, SEXP lat)
{
    PROTECT(lon = AS_NUMERIC(lon));
    double *lonp = REAL(lon);
    PROTECT(lat = AS_NUMERIC(lat));
    double *latp = REAL(lat);
    int nlat = length(lat);
    int nlon = length(lon);
    if (nlon < 1) error("must have at least 2 longitudes");
    if (nlat < 1) error("must have at least 2 latitudes");
    int n = nlon * nlat;
    SEXP polylon; 
    PROTECT(polylon = allocVector(REALSXP, 5*n));
    double *polylonp = REAL(polylon);
    SEXP polylat;
    PROTECT(polylat = allocVector(REALSXP, 5*n));
    double *polylatp = REAL(polylat);
    int k = 0;
    double latstep = 0.5 * (latp[1] - latp[0]);
    double lonstep = 0.5 * (lonp[1] - lonp[0]);
#ifdef DEBUG
    Rprintf("nlon: %d, nlat: %d, latstep: %f, lonstep: %f\n", nlon, nlat, latstep, lonstep);
#endif
    for (int i = 0; i < nlon; i++) {
        for (int j = 0; j < nlat; j++) {
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
    PROTECT(res = allocVector(VECSXP, 2));
    PROTECT(res_names = allocVector(STRSXP, 2));
    SET_VECTOR_ELT(res, 0, polylon);
    SET_STRING_ELT(res_names, 0, mkChar("longitude"));
    SET_VECTOR_ELT(res, 1, polylat);
    SET_STRING_ELT(res_names, 1, mkChar("latitude"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(6);
    return(res);
}


SEXP map_check_polygons(SEXP x, SEXP y, SEXP xokspan) // returns new x vector
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(xokspan = AS_NUMERIC(xokspan));
    double *xp = REAL(x);
    double *yp = REAL(y);
    double *xokspanp = REAL(xokspan);
    int nx = length(x);
    int ny = length(y);
    if (nx < 1) error("must have at least 2 x values");
    if (ny < 1) error("must have at least 2 y values");
    SEXP xout;
    PROTECT(xout = allocVector(REALSXP, nx));
    double *xoutp = REAL(xout);
    int npoly = nx / 5;

    SEXP okPoint, okPolygon;
    PROTECT(okPolygon = allocVector(LGLSXP, npoly)); 
    PROTECT(okPoint = allocVector(LGLSXP, nx)); 
    int *okPointp = INTEGER(okPoint);
    int *okPolygonp = INTEGER(okPolygon);
    for (int ipoly = 0; ipoly < npoly; ipoly++) {
        okPolygonp[ipoly] = 1;
    }
    for (int ix = 0; ix < nx; ix++) {
        xoutp[ix] = xp[ix];
        okPointp[ix] = 1;
    }
    // x1 x2 x3 x4 NA x1 x2 x3 x4 NA ...
    double dxPermitted = fabs(*xokspanp);
    Rprintf("dxPermitted: %f\n", dxPermitted);
    int count = 0, ncount=10; // FIXME: remove when working
    for (int ipoly = 0; ipoly < npoly; ipoly++) {
        int badPolygon;
        int start = 5 * ipoly;
        badPolygon = 0;
        // Discard polygons containing NA for x or y
        for (int j = 0; j < 4; j++) { // skip 5th point which is surely NA
            if (ISNA(xp[start + j]) || ISNA(yp[start + j])) {
                for (int k = 0; k < 5; k++) {
                    xoutp[start + k] = 0.0;
                    okPointp[start + k] = 0;
                }
                okPolygonp[ipoly] = 0;
                badPolygon = 1;
                break;
            }
        }
        if (badPolygon)
            continue;
        for (int j = 1; j < 4; j++) {
            if (dxPermitted < fabs(xp[start + j] - xp[start + j - 1])) {
                if (count < ncount) { // FIXME: remove when working
                    Rprintf("ipoly: %d, j: %d, span: %f (limit to span: %f)\n",
                            ipoly, j, fabs(xp[start+j]-xp[start+j-1]), dxPermitted);
                }
                for (int k = 0; k < 5; k++) {
                    xoutp[start + k] = 0.0;
                    okPointp[start + k] = 0;
                }
                okPolygonp[ipoly] = 0;
                break;
            }
        }
    }
    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 3));
    PROTECT(res_names = allocVector(STRSXP, 3));
    SET_VECTOR_ELT(res, 0, xout);
    SET_STRING_ELT(res_names, 0, mkChar("x"));

    SET_VECTOR_ELT(res, 1, okPoint);
    SET_STRING_ELT(res_names, 1, mkChar("okPoint"));
    setAttrib(res, R_NamesSymbol, res_names);

    SET_VECTOR_ELT(res, 2, okPolygon);
    SET_STRING_ELT(res_names, 2, mkChar("okPolygon"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(8);
    return(res);
}

