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

system("R CMD SHLIB map.c") 
dyn.load("map.so")
poly <- .Call("map_assemble_polygons", lon, lat)
drawPalette(Tlim, col=oceColorsJet)
mapPlot(coastlineWorld, projection='mollweide', grid=FALSE)
xy <- mapproject(poly$longitude, poly$latitude)
pal <- oceColorsJet(100)
plot(range(xy$x, na.rm=TRUE), range(xy$y, na.rm=TRUE), type='n', asp=1, xlab="", ylab="", axes=FALSE)
ok <- .Call("map_find_bad_polygons", xy$x, xy$y, diff(par('usr'))[1:2]/5)
i<-20702+seq(-10,10); data.frame(i=i,ok=ok[i],x=xy$x[i],y=xy$y[i])
polygon(ok, xy$y, col=pal[rescale(as.vector(SST),Tlim[1],Tlim[2],1,100)],border=NA)



*/

SEXP map_assemble_polygons(SEXP lon, SEXP lat)
{
    PROTECT(lon = AS_NUMERIC(lon));
    PROTECT(lat = AS_NUMERIC(lat));
    int nlat = length(lat);
    int nlon = length(lon);
#ifdef DEBUG
    Rprintf("nlon: %d, nlat: %d\n", nlon, nlat);
#endif
    if (nlon < 1) error("must have at least 2 longitudes");
    if (nlat < 1) error("must have at least 2 latitudes");
    int n = nlon * nlat;
    SEXP polylon; 
    SEXP polylat;
    PROTECT(polylon = allocVector(REALSXP, 5*n));
    PROTECT(polylat = allocVector(REALSXP, 5*n));
    double *lonp = REAL(lon);
    double *latp = REAL(lat);
    double *polylonp = REAL(polylon);
    double *polylatp = REAL(polylat);
    int k = 0;
    double latstep = 0.5 * (latp[1] - latp[0]);
    double lonstep = 0.5 * (lonp[1] - lonp[0]);
    //Rprintf("latstep: %f, lonstep: %f\n", latstep, lonstep);
    for (int j = 0; j < nlat; j++) {
        for (int i = 0; i < nlon; i++) {
#ifdef DEBUG
            Rprintf("i: %d, j: %d, lon: %.1f, lat:%.1f, k: %d\n", i, j, lonp[i], latp[j], k);
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
    SET_VECTOR_ELT(res, 1, polylat);
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(6);
    return(res);
}


SEXP map_find_bad_polygons(SEXP x, SEXP y, SEXP xokspan)
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(xokspan = AS_NUMERIC(xokspan));
    double *xp = REAL(x);
    double *xokspanp = REAL(xokspan);
    int nx = length(x);
    int ny = length(y);
    if (nx < 1) error("must have at least 2 x values");
    if (ny < 1) error("must have at least 2 y values");
    SEXP res;
    PROTECT(res = allocVector(REALSXP, nx)); // new x
    double *resp = REAL(res);
    int lastNA = -1;
    resp[0] = 1;
    int examples = 0;
    for (int i = 0; i < nx; i++)
        resp[i] = NA_REAL;
    for (int i = 1; i < nx; i++) {
        if (ISNA(xp[i])) {
            resp[i] = NA_REAL;
            lastNA = i;
        } else {
            double dx = fabs(xp[i] - xp[i-1]);
            if (dx > *xokspanp) {
                // x1 x2 x3 x4 NA x1 x2 x3 x4 NA ...
                // Use sign from first element
                int sign = xp[lastNA+1] > 0.0;
                i = lastNA + 1;
                for (int j = lastNA + 1; j < lastNA + 5; j++) {
                    resp[j] = sign * fabs(xp[j]);
#ifdef DEBUG
                    if (examples < 1)
                        Rprintf("i: %d, j: %d, x: %f set to NA (lastNA was %d); sign %d res %f\n",
                                i, j, xp[j], lastNA, sign, resp[j]);
#endif
                    i++;
                }
                lastNA = i;
#ifdef DEBUG
                if (examples++ < 1)
                    Rprintf("set lastNA to %d\n", lastNA);
#endif
            } else {
                resp[i] = xp[i];
            }
        }
    }
    UNPROTECT(4);
    return(res);
}

