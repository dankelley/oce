/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <math.h>
#include <Rdefines.h>
#include <Rinternals.h>

void geoddist_core(double *lat1, double *lon1, double *lat2, double *lon2, double *a, double *f, double *faz, double *baz, double *s);

/*
   library(oce) 
   a <- 6378137.00          # WGS84 major axis
   f <- 1/298.257223563     # WGS84 flattening parameter
   n <- length(lat1)
   system("R CMD SHLIB geod.c"); dyn.load("geod.so");
   lat1 <- 0
   lon1 <- 0
   lat2 <- 1
   lon2 <- 0

# Note that the (lon/lat) order is different from C to R. Also, the C returns metres,
# whereas the R returns km.
.Call("geoddist",lat1,lat1,lat2,lon2,a,f,dist=double(length(lat1)))
# [1] 110574.4
geodDist(lon1,lat1,lon2,lat2)
# [1] 110.5744

*/

SEXP geoddist_alongpath(SEXP lat, SEXP lon, SEXP a, SEXP f)
{
  if (!isReal(lat))
    error("latitude must be a numeric (floating-point) vector");
  if (!isReal(lon))
    error("longitude must be a numeric (floating-point) vector");
  SEXP res;
  //int n = INTEGER(GET_LENGTH(lat));
  //int nlon = INTEGER(GET_LENGTH(lon));
  int n = GET_LENGTH(lat);
  int nlon = GET_LENGTH(lon);
  if (n != nlon)
    error("lengths of latitude and longitude vectors must match, but they are %d and %d, respectively", n, nlon);
  double *latp = REAL(lat);
  double *lonp = REAL(lon);
  double *ap = REAL(a);
  double *fp = REAL(f);
  PROTECT(res = allocVector(REALSXP, n));
  double *resp = REAL(res);
  double last = 0.0;
  resp[0] = ISNA(lonp[0]) ? NA_REAL : 0.0;
  for (int i = 0; i < n-1; i++) {
    double faz, baz, s;
    if (ISNA(latp[i]) || ISNA(lonp[i]) || ISNA(latp[i+1]) || ISNA(lonp[i+1])) {
      resp[i+1] = NA_REAL;
      last = 0.0; // reset
    } else {
      geoddist_core(latp+i, lonp+i, latp+i+1, lonp+i+1, ap, fp, &faz, &baz, &s);
      resp[i+1] = last + s;
      last = resp[i+1];
    }
  }
  UNPROTECT(1);
  return(res);
}

SEXP geoddist(SEXP lat1, SEXP lon1, SEXP lat2, SEXP lon2, SEXP a, SEXP f)
{
  if (!isReal(lat1)) error("lat1 must be a numeric (floating-point) vector");
  if (!isReal(lon1)) error("lon1 must be a numeric (floating-point) vector");
  if (!isReal(lat2)) error("lat2 must be a numeric (floating-point) vector");
  if (!isReal(lon2)) error("lon2 must be a numeric (floating-point) vector");
  int n = GET_LENGTH(lat1);
  if (n != GET_LENGTH(lon1))
    error("lengths of lat1 and lon1 must match, but they are %d and %d respectively.", n, GET_LENGTH(lon1));
  if (n != GET_LENGTH(lat2))
    error("lengths of lat1 and lat2 must match, but they are %d and %d respectively.", n, GET_LENGTH(lat2));
  if (n != GET_LENGTH(lon2))
    error("lengths of lon1 and lon2 must match, but they are %d and %d respectively.", n, GET_LENGTH(lon2));
  double *lat1p = REAL(lat1);
  double *lon1p = REAL(lon1);
  double *lat2p = REAL(lat2);
  double *lon2p = REAL(lon2);
  double *ap = REAL(a);
  double *fp = REAL(f);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, n));
  double *resp = REAL(res);
  for (int i = 0; i < n; i++) {
    double faz, baz, s;
    geoddist_core(lat1p+i, lon1p+i, lat2p+i, lon2p+i, ap, fp, &faz, &baz, &s);
    resp[i] = s;
  }
  UNPROTECT(1);
  return(res);
}

void geoddist_core(double *lat1, double *lon1, double *lat2, double *lon2, double *a, double *f,
    double *faz, double *baz, double *s)
{
  /* Solution of the geodetic inverse problem according to [^1].

     lat and lon = conventional, in degrees

     a = semi-major axis of the reference ellipsoid.

     f = flattening of the ref ellipsoid.

     - Programmed for cdc-6600 by LCdr L.Pfeifer NGS Rockville MD 18feb75
     - Modified for ibm system 360 by john g gergen ngs rockville md 7507
     - Modified for R by D.Gillis Zoology University of Manitoba 16JUN03.
     - Translated from fortran to C by Dan Kelley, Dalhousie University 2009-04.

     [1]: Vincenty,T. 1975. Direct and inverse solutions of
     geodesics on the ellipsoid with application of nested
     equations. Survey Review 23(176):88-94.
     */

  double eps = 0.5e-13;
  double pi = M_PI;
  double rpd = M_PI / 180.0; // radians per degree
  double r = 1.0 - (*f);
  double glat1, glon1, glat2, glon2, tu1, tu2, cu1, su1, cu2, x, sx, cx, sy, cy, y, sa, c2a, cz, e, c, d;
  double lon1copy = *lon1, lon2copy = *lon2;
  int iter;
  if (((*lat1) == (*lat2)) && ((*lon1) == (*lon2))) {
    *s = 0.0;
    *faz = 0.0;
    *baz = 0.0;
    return;
  }

  if ((*lon1) < 0)
    *lon1 += 360.0;
  if ((*lon2) < 0)
    *lon2 += 360.0;

  glat1 = (*lat1) * rpd;
  glon1 = (*lon1) * rpd;
  glat2 = (*lat2) * rpd;
  glon2 = (*lon2) * rpd;

  tu1 = r * sin(glat1) / cos(glat1);
  tu2 = r * sin(glat2) / cos(glat2);
  cu1 = 1.0 / sqrt(tu1 * tu1 + 1.0);
  su1 = cu1 * tu1;
  cu2 = 1.0 / sqrt(tu2 * tu2 + 1.0);
  *s = cu1 * cu2;
  *baz = (*s) * tu2;
  *faz = (*baz) * tu1;
  x = glon2 - glon1;
  iter = 1;
  do {
    sx = sin(x);
    cx = cos(x);
    tu1 = cu2 * sx;
    tu2 = (*baz) - su1 * cu2 * cx;
    sy = sqrt(tu1 * tu1 + tu2 * tu2);
    cy = (*s) * cx + (*faz);
    y = atan2(sy, cy);
    sa = (*s) * sx / sy;
    c2a = -sa * sa + 1.0;
    cz = 2.0 * (*faz);
    if(c2a > 0.0)
      cz = -cz / c2a + cy;
    e = cz * cz * 2.0 - 1.0;
    c = ((-3.0 * c2a + 4.0) * (*f) + 4.0) * c2a * (*f) / 16.0;
    d = x;
    x = ((e * cy * c + cz) * sy * c + y) * sa;
    x = (1.0 - c) * x * (*f) + glon2 - glon1;
  } while(fabs(d - x) > eps && iter++ < 10);
  *faz = atan2(tu1, tu2);
  *baz = atan2(cu1 * sx, (*baz) * cx - su1 * cu2) + pi;
  x = sqrt((1.0 / r / r - 1.0) * c2a + 1.0) + 1.0;
  x = (x - 2.0) / x;
  c = 1.0 - x;
  c = (x * x / 4.0 + 1.0) / c;
  d = (0.375 * x * x - 1.0)*x;
  x = e * cy;
  *s = 1.0 - e - e;
  *s = ((((sy * sy * 4.0 - 3.0) * (*s) * cz * d / 6.0 - x) * d / 4.0 + cz) * sy * d + y) * c * (*a) * r;
  *faz = (*faz) / rpd;
  *baz = (*baz) / rpd;
  *lon1 = lon1copy;
  *lon2 = lon2copy;
}

void geod_xy(int *n,
    double *lat,		/* vector of latitudes */
    double *lon,		/* vector of longitudes */
    double *latr,		/* single reference latitude */
    double *lonr,		/* single reference longitude */
    double *a,		/* WGS84 major axis 6378137.00 */
    double *f,    /* WGS84 flattening parameter 1/298.257223563 */
    double *x, double *y)
{
  double rpd = M_PI / 180.;
  double faz, baz, s;
  for (int i = 0; i < *n; i++) {
    geoddist_core(lat++,
	lon++,
	latr, 
	lonr,
	a,
	f,
	&faz,
	&baz,
	&s);
    *x++ = s * sin(baz * rpd);
    *y++ = s * cos(baz * rpd);
  }
}
