// assemble_polygons(): construct lat-lon polygons, hopefully to speed up mapImage()
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/*
   

system("R CMD SHLIB testing.c") 
dyn.load("testing.so")
D <- .Call("assemble_polygons", c(0, 1, 2), c(10, 11, 12))
plot(D$longitude, D$latitude)
polygon(D$longitude, D$latitude, col=rainbow(4))


*/

SEXP assemble_polygons(SEXP lon, SEXP lat)
{
    PROTECT(lon = AS_NUMERIC(lon));
    PROTECT(lat = AS_NUMERIC(lat));
    int nlat = length(lat);
    int nlon = length(lon);
#ifdef DEBUG
    Rprintf("nlon: %d, nlat: %d\n", nlon, nlat);
#endif
    SEXP polylon; 
    SEXP polylat;
    if (nlon < 1) error("must have at least 2 longitudes");
    if (nlat < 1) error("must have at least 2 latitudes");
    int n = (nlon - 1) * (nlat - 1);
    PROTECT(polylon = allocVector(REALSXP, 5*n));
    PROTECT(polylat = allocVector(REALSXP, 5*n));
    double *lonp = REAL(lon);
    double *latp = REAL(lat);
    double *polylonp = REAL(polylon);
    double *polylatp = REAL(polylat);
    polylonp[0] = 0.0;
    polylonp[1] = 1.0;
    polylatp[0] = 0.0;
    polylatp[1] = 1.0;
    int klon = 0;
    int klat = 0;
    for (int i = 1; i < nlon; i++) {
        for (int j = 1; j < nlat; j++) {
#ifdef DEBUG
            Rprintf("i: %d, j: %d, lon: %.1f, lat:%.1f\n", i, j, lonp[i], latp[j]);
#endif
            // Lower left
            polylonp[klon++] = lonp[i-1];
            polylatp[klat++] = latp[j-1];
            // Upper left
            polylonp[klon++] = lonp[i-1];
            polylatp[klat++] = latp[j];
            // Upper right
            polylonp[klon++] = lonp[i];
            polylatp[klat++] = latp[j];
            // Lower right
            polylonp[klon++] = lonp[i];
            polylatp[klat++] = latp[j-1];
            // end
            polylonp[klon++] = NA_REAL;
            polylatp[klat++] = NA_REAL;
#ifdef DEBUG
            Rprintf("   klon: %d, klat: %d\n", klon, klat);
#endif
        }
    }
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

