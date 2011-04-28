#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*

To test this, without building the whole package, do the following.

library(oce)
lat <-  (44+39/60) + seq(-0.166666, 0.166666, 0.0166666)
lon <- -(63+36/60) + rep(0, length(lat))
data(topoMaritimes)

system("R CMD SHLIB topo_interpolate.c")
dyn.load("topo_interpolate.so")
z <- .Call("topo_interpolate", lat, lon, topoMaritimes$data$latitude, topoMaritimes$data$longitude, topoMaritimes$data$z)
plot(lat, z, type='l')
*/

#define DEBUG

SEXP topo_interpolate(SEXP lat, SEXP lon, SEXP grid_lat, SEXP grid_lon, SEXP grid_z)
{
    PROTECT(lat = AS_NUMERIC(lat));
    PROTECT(lon = AS_NUMERIC(lon));
    PROTECT(grid_lat = AS_NUMERIC(grid_lat));
    PROTECT(grid_lon = AS_NUMERIC(grid_lon));
    PROTECT(grid_z = AS_NUMERIC(grid_z));
    int lat_len = length(lat);
    int lon_len = length(lon);
    if (lat_len != lon_len)
        error("lengths of latitude (%d) and longitude (%d) must match", lat_len, lon_len);
    int grid_lat_len = length(grid_lat);
    if (grid_lat_len < 2)
        error("topo grid must have at least 2 latitudes");
    int grid_lon_len = length(grid_lon);
    if (grid_lon_len < 2)
        error("topo grid must have at least 2 longitudes");
    int grid_z_len = length(grid_z);
#ifdef DEBUG
    Rprintf("lat_len=%d lon_len=%d\n", lat_len, lon_len);
    Rprintf("grid_lat_len=%d grid_lon_len=%d grid_z_len=%d\n", grid_lat_len, grid_lon_len, grid_z_len);
#endif
    SEXP ans;
    PROTECT(ans = allocVector(REALSXP, lat_len));
    double *ansp;
    ansp = REAL(ans);
    // we may assume a regular grid
    double *latp, *lonp, *grid_latp, *grid_lonp, *grid_zp;
    latp = REAL(lat);
    lonp = REAL(lon);
    grid_latp = REAL(grid_lat);
    grid_lonp = REAL(grid_lon);
    grid_zp = REAL(grid_z);
    double grid_lat_increment = grid_latp[1] - grid_latp[0];
    double grid_lon_increment = grid_lonp[1] - grid_lonp[0];
    for (int i = 0; i < lat_len; i++) {
        int glati = (int)floor((latp[i] - grid_latp[0]) / grid_lat_increment);
        if (glati < 0 || glati > grid_lat_len - 1) {
            ansp[i] = NA_REAL;
        } else {
            int gloni = (int)floor((lonp[i] - grid_lonp[0]) / grid_lon_increment);
            if (gloni < 0 || gloni > grid_lon_len - 1) {
                ansp[i] = NA_REAL;
            } else {
                int look = glati + grid_lat_len * gloni;
#ifdef DEBUG
                Rprintf("lat %f lon %f look %d\n", latp[i], lonp[i], look);
#endif
                if (look < 0 || look > grid_z_len - 1)
                    ansp[i] = NA_REAL;
                else 
                    ansp[i] = grid_zp[look];
            }
        }
    }
    Rprintf("first e grids %f %f %f\n", grid_zp[0], grid_zp[1], grid_zp[2]);
    UNPROTECT(6);
    return(ans);
}
#if 0
int x_len = length(x);
  int  y_len = length(y);
  int xout_len = length(xout);
  double *xp, *yp, *xoutp, *ansp;
  SEXP ans;
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(xout = AS_NUMERIC(xout));
  if (x_len != y_len) error("lengths of x (%d) and y (%d) disagree", x_len, y_len);
  xp = REAL(x);
  xoutp = REAL(xout);
  yp = REAL(y);
  PROTECT(ans = allocVector(REALSXP, xout_len));
  ansp = REAL(ans);
  int i;
#if 0
  Rprintf("DEBUG: x="); for (i = 0; i < x_len; i++) Rprintf("%f ", *(xp + i));  Rprintf("\n");
  Rprintf("DEBUG: y="); for (i = 0; i < x_len; i++) Rprintf("%f ", *(yp + i));  Rprintf("\n");
  Rprintf("DEBUG: xout="); for (i = 0; i < xout_len; i++) Rprintf("%f ", *(xoutp + i));  Rprintf("\n");
#endif
  for (i = 0; i < xout_len; i++) {
    //Rprintf("xout[%d] = %f\n",i,*(xoutp+i));
    int j;
    double val;
    int found;
    found = 0;
    //Rprintf("x[%d]=%.1f...", j, *(xp + j));
    for (j = 0; j < x_len - 1; j++) {
      double xx = *(xoutp + i);
      //Rprintf("%.1f", *(xp + j));
      // Look for neighbors (BUG: what about hitting directly?)
      if (xx == *(xp + j)) {
        val = *(yp + j);
        found = 1;
      } else if (xx == *(xp + j + 1)) {
        val = *(yp + j + 1);
        found = 1;
      } else if (*(xp + j) < xx && xx < *(xp + j + 1)) {
        if (j == 0) {           /* catch exact match (just in case there is a problem with such) */
          val = *yp + (xx - *xp) * (*(yp + 1) - *(yp)) / (*(xp + 1) - *xp);
          //Rprintf("j=0 ... xx=%f yields val=%f since x[0,1]=%f , %f have y[0,1]=%f , %f\n", xx, val, *xp, *(xp+1), *yp,*(yp+1));
        } else if (j == x_len - 1) {
          val = *(yp + j - 1) + (xx - *(xp + j - 1)) * (*(yp + j) - *(yp + j - 1)) / (*(xp + j) - *(xp + j - 1));
        } else {
          val = phi_z(j, xx, xp, yp, x_len);
        }
        //Rprintf("Y j=%d VAL=%f\n", j, val);
        found = 1;
        break;
      }
    }
    if (found) 
      *(ansp + i) = val;
    else 
      *(ansp + i) = NA_REAL;
  }
  UNPROTECT(4);
  return(ans);
}
#endif

