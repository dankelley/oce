#include <R.h>
#include <math.h>
#include <Rdefines.h>
#include <Rinternals.h>

void geoddist_core(double *lat1, double *lon1, double *lat2, double *lon2, double *a, double *f, double *faz, double *baz, double *s);

/*
library(oce) 
data(section)
lon1 <- section[["longitude", "byStation"]]
lat1 <- section[["latitude", "byStation"]]
a <- 6378137.00          # WGS84 major axis
f <- 1/298.257223563     # WGS84 flattening parameter
n <- length(lat)
system("R CMD SHLIB geod.c"); dyn.load("geod.so");
lon2 <- lon1
lat2 <- lat1 + 1

.Call("geoddist", lat1, lon1, lat2, lon2, a, f, dist=double(length(lon2)))
#[1] 111136.0 111135.8 111135.6 111135.2 111135.1 111135.0 111134.9 111134.3

geodDist(lat1,lon1,lat2,lon2)
#[1] 111.1360 111.1358 111.1356 111.1352 111.1351 111.1350 111.1349 111.1343

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
        error("lengths of lat2 and lat2 must match, but they are %d and %d respectively.", n, GET_LENGTH(lat2));
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
	/*
	 Solution of the geodetic inverse problem after t.vincenty
	 modified Rainsford's method with hHlmert's elliptical terms
	 effective in any azimuth and at any distance short of antipodal
	 standpoint/forepoint must not be the geographic pole.

	 "a" is the semi-major axis of the reference ellipsoid.

	 "f" is the flattening (not reciprocal) of the refernece ellipsoid.

	 Latitudes and longitudes in radians positive north and east
	 forward azimuths at both points returned in radians from north.
       
	 Programmed for cdc-6600 by LCdr L.Pfeifer NGS Rockville MD 18feb75
	 modified for ibm system 360 by john g gergen ngs rockville md 7507

	 Modified for R by D.Gillis Zoology University of Manitoba 16JUN03.

	 Translated from fortran to C by Dan Kelley, Dalhousie
	 University 2009-04.

	 Replaced common blocks for constants with DATA statements.  Datum
	 parameters moved from common block to subroutine arguements.
     
	 *** Input Variables: DLAT1,DLON1 - initial fix (P1) in degrees
	 (latitude, north +) (longitude east +)
	 DLAT2,DLON2 - destination fix (P2) in degrees
	 
	 Ellipsoid (spheroid model, eg. WGS84)
	 A   - radius of major axis in distance units
	 F   - flattening factor
	 
	 *** Output Variables: FAZ - azimuth of the geodesic (P1 to P2)
	 BAZ - azimuth of the geodesic (P2 to P1)
	 S   - spheroidal distance = length of the geodesic
	 in distance units
     
	 After Vincenty,T. 1975. Direct and inverse solutions of
	 geodesics on the ellipsoid with application of nested
	 equations. Survey Review 23(176):88-94.
	*/
      
	double eps = 0.5e-13;
	double pi = 3.1415926535897932384626433832795;
	double rad = 0.0174532925199432957692369076848861;
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

	glat1 = (*lat1) * rad;
	glon1 = (*lon1) * rad;
	glat2 = (*lat2) * rad;
	glon2 = (*lon2) * rad;

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
	*faz = (*faz) / rad;
	*baz = (*baz) / rad;
        *lon1 = lon1copy;
        *lon2 = lon2copy;
}

void
geod_xy(int *n,
	double *lat,		/* vector of latitudes */
	double *lon,		/* vector of longitudes */
	double *latr,		/* single reference latitude */
	double *lonr,		/* single reference longitude */
	double *a,		/* WGS84 major axis 6378137.00 */
	double *f,    /* WGS84 flattening parameter 1/298.257223563 */
	double *x, double *y)
{
	double rad = 0.0174532925199432957692369076848861;
	double faz, baz, s;
	int i;
	for (i = 0; i < *n; i++) {
		geoddist_core(lat++,
			 lon++,
			 latr, 
			 lonr,
			 a,
			 f,
			 &faz,
			 &baz,
			 &s);
		*x++ = s * sin(baz * rad);
		*y++ = s * cos(baz * rad);
	}
}
