// /Library/Frameworks/R.framework/Versions/Current/Resources/library/proj4/libs/proj4.so
#include <stdio.h>
#include <string.h>
#include <R.h>
#include "proj4/proj_api.h"
#define d2r 57.29577951
// #include "proj_config.h"

/*
 
   library(oce)
   lon <- seq(0, 10, 1)
   lat <- seq(1, 11, 1)
   proj <- "+proj=merc +ellps=WGS84"
   n <- length(lon)
   res <- .C("proj4_test1", as.character(proj), as.integer(n), as.double(lon), as.double(lat), x=double(n), y=double(n))
# see res$x and res$y

 */

// FIXME: break into three parts: setup, forward and inverse; then hook up to existing code.

void proj4_test1(char **proj_spec, int *n, double *lon, double *lat, double *x, double *y)
{
    //Rprintf("%s\n", *proj_spec);
    projPJ pj;
    projUV lonlat, xy, lonlat2;
    if (!(pj = pj_init_plus(*proj_spec))) 
        error("ERROR %s\n", pj_strerrno(*pj_get_errno_ref()));
    //printf("%s\n", proj_spec);
    for (int i=0; i< (*n); i++) {
        //printf("lon=%f lat=%f", lon[i], lat[i]);
        lonlat.u = lon[i]/d2r;
        lonlat.v = lat[i]/d2r;
        xy = pj_fwd(lonlat, pj);
        x[i] = xy.u;
        y[i] = xy.v;
        //printf(" x=%.1f y=%.1f", x[i], y[i]);
    }
    pj_free(pj);
}


