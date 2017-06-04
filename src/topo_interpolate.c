#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*

# Below is R code to test (without building package)

library(oce)
library(ocedata)
data(topoMaritimes)
lat <- seq(43, 53, 1/60/10)
lon <- -(63+36/60) + rep(0, length(lat)) # Halifax
system("R CMD SHLIB topo_interpolate.c")
dyn.load("topo_interpolate.so")
z <- .Call("topo_interpolate", lat, lon, topoMaritimes$data$latitude, topoMaritimes$data$longitude, topoMaritimes$data$z)
plot(lat, z, type='l')
abline(h=0, col='blue') # mean sea level
coasts <- c(44.4906, 45.88075, 46.22400, 46.55009, 49.43337, 49.86243, 50.18852) # from map
abline(v=coasts, col='red')

*/

//#define DEBUG

#define look(lati, loni) ((loni) + grid_lon_len * (lati))

SEXP topo_interpolate(SEXP lat, SEXP lon, SEXP grid_lat, SEXP grid_lon, SEXP grid_z)
    // bilinear interpolation on (regular) topo grid
{
    PROTECT(lat = AS_NUMERIC(lat));
    PROTECT(lon = AS_NUMERIC(lon));
    int lat_len = length(lat);
    int lon_len = length(lon);
    if (lat_len != lon_len)
        error("lengths of latitude (%d) and longitude (%d) must match", lat_len, lon_len);
    PROTECT(grid_lat = AS_NUMERIC(grid_lat));
    PROTECT(grid_lon = AS_NUMERIC(grid_lon));
    int grid_lat_len = length(grid_lat);
    if (grid_lat_len < 2)
        error("topo grid must have at least 2 latitudes");
    int grid_lon_len = length(grid_lon);
    if (grid_lon_len < 2)
        error("topo grid must have at least 2 longitudes");
    PROTECT(grid_z = AS_NUMERIC(grid_z));
    int grid_z_len = length(grid_z);
    SEXP ans;
    PROTECT(ans = allocVector(REALSXP, lat_len));
    // Last letter "p" indicates a pointer.
    double *latp, *lonp, *grid_latp, *grid_lonp, *grid_zp, *ansp;
    latp = REAL(lat);
    lonp = REAL(lon);
    grid_latp = REAL(grid_lat);
    grid_lonp = REAL(grid_lon);
    grid_zp = REAL(grid_z);
    ansp = REAL(ans);
    // It is safe to assume a uniform grid, so next two calculations apply throughout
    double grid_lat_increment = grid_latp[1] - grid_latp[0];
    double grid_lon_increment = grid_lonp[1] - grid_lonp[0];
    for (int i = 0; i < lat_len; i++) {
        int grid_lat_index, grid_lon_index;
        grid_lat_index = (int)floor((latp[i] - grid_latp[0]) / grid_lat_increment);
        if (grid_lat_index < 0 || grid_lat_index > grid_lat_len - 1) {
#ifdef DEBUG
            Rprintf("NA #A: latp[%d]=%lf, grid_latp[0]=%lf, grid_lat_index=%d, grid_lat_len=%d\n",
                    i, latp[i], grid_latp[0], grid_lat_index, grid_lat_len);
#endif
            ansp[i] = NA_REAL;
        } else {
            grid_lon_index = (int)floor((lonp[i] - grid_lonp[0]) / grid_lon_increment);
            if (grid_lon_index < 0 || grid_lon_index > grid_lon_len - 1) {
#ifdef DEBUG
                Rprintf("NA #B: i=%d, grid_lon_index=%d, grid_lon_len=%d\n", i, grid_lon_index, grid_lon_len);
#endif
                ansp[i] = NA_REAL;
            } else {
                int l = look(grid_lat_index, grid_lon_index);
                if (l < 0 || l > grid_z_len - 1) {
#ifdef DEBUG
                    Rprintf("NA #C: i=%d, l=%d, grid_z_len=%d\n", i, l, grid_z_len);
#endif
                    ansp[i] = NA_REAL;
                } else {
                    // http://en.wikipedia.org/wiki/Bilinear_interpolation
                    double x = (lonp[i] - grid_lonp[grid_lon_index]) / grid_lon_increment;
                    double y = (latp[i] - grid_latp[grid_lat_index]) / grid_lat_increment;
                    double zll = grid_zp[l];
                    double zlr = grid_zp[look(grid_lat_index  , grid_lon_index+1)];
                    double zur = grid_zp[look(grid_lat_index+1, grid_lon_index+1)];
                    double zul = grid_zp[look(grid_lat_index+1, grid_lon_index  )];
                    ansp[i] = zll * (1.0 - x) * (1.0 - y) +
                        zlr * x * (1.0 - y) +
                        zul * (1.0 - x) * y +
                        zur * x * y;
#ifdef DEBUG
                    if (i < 50)
                        Rprintf("lat %.3f [%d] lon %.3f [%d] | ll %.1f [%4d] lr %.1f [%4d] ur %.1f [%4d] ul %.1f [%4d] | xy %f %f | ans %.1f\n",
                                latp[i], grid_lat_index, lonp[i], grid_lon_index,
                                l, zll,
                                look(grid_lat_index  , grid_lon_index+1), zll,
                                look(grid_lat_index+1, grid_lon_index+1), zur,
                                look(grid_lat_index+1, grid_lon_index  ), zul,
                                x, y, 
                                ansp[i]);
#endif
                }
            }
        }
    }
#ifdef DEBUG
    Rprintf("grid_lat_len %d    grid_lon_len %d\n", grid_lat_len, grid_lon_len);
    Rprintf("latp      starts %9.4f %9.4f %9.4f\n", latp[0], latp[1], latp[2]);
    Rprintf("grid_latp starts %9.4f %9.4f %9.4f\n", grid_latp[0], grid_latp[1], grid_latp[2]);
    Rprintf("lonp      starts %9.4f %9.4f %9.4f\n", lonp[0], lonp[1], lonp[2]);
    Rprintf("grid_lonp starts %9.4f %9.4f %9.4f\n", grid_lonp[0], grid_lonp[1], grid_lonp[2]);
    Rprintf("grid_zp   starts %9.4f %9.4f %9.4f\n", grid_zp[0], grid_zp[1], grid_zp[2]);
#endif
    UNPROTECT(6);
    return(ans);
}
