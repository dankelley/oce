/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>

// This code is intended to speed up bin-mapping calculations.  Since
// the core of the method relies on using approx(), the first part
// of the code here is copied from R sources listed below.
// Then (after a line of slashes), the actual code for bin mapping appears.
//
// R SOURCES USED:
//   src/library/stats/src/approx.c 

//////////////////////////////////////////////////////////////////////


/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2001   Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */



/* Linear and Step Function Interpolation */

/* Assumes that ordinates are in ascending order
 * The right interval is found by bisection
 * Linear/constant interpolation then takes place on that interval
*/

/* NB:  R_interv(.) in ./interv.c  is conceptually a special case of
 *	this, where y = 1:n */

typedef struct {
    double ylow;
    double yhigh;
    double f1;
    double f2;
    int kind;
} appr_meth;

static double approx1(double v, double *x, double *y, int n,
		      appr_meth *Meth)
{
    /* Approximate  y(v),  given (x,y)[i], i = 0,..,n-1 */
    int i, j, ij;

    if(!n) return R_NaN;

    i = 0;
    j = n - 1;

    /* handle out-of-domain points */

    if(v < x[i]) return Meth->ylow;
    if(v > x[j]) return Meth->yhigh;

    /* find the correct interval by bisection */

    while(i < j - 1) { /* x[i] <= v <= x[j] */
	ij = (i + j)/2; /* i+1 <= ij <= j-1 */
	if(v < x[ij]) j = ij;
	else i = ij;
	/* still i < j */
    }
    /* provably have i == j-1 */

    /* interpolation */

    if(v == x[j]) return y[j];
    if(v == x[i]) return y[i];
    /* impossible: if(x[j] == x[i]) return y[i]; */

    if(Meth->kind == 1) { /* linear */
	return y[i] + (y[j] - y[i]) * ((v - x[i])/(x[j] - x[i]));
    }
    else { /* 2 : constant */
	return y[i] * Meth->f1 + y[j] * Meth->f2;
    }
}/* approx1() */


	/* R Frontend for Linear and Constant Interpolation */

void R_approx(double *x, double *y, int *nxy, double *xout, int *nout,
	      int *method, double *yleft, double *yright, double *f)
{
    int i;
    appr_meth M = {0.0, 0.0, 0.0, 0.0, 0}; /* -Wall */

    /* check interpolation method */

    switch(*method) {
    case 1: /* linear */
	break;
    case 2: /* constant */
	if(!R_FINITE(*f) || *f < 0 || *f > 1)
	    error("approx(): invalid f value");
	M.f2 = *f;
	M.f1 = 1 - *f;
	break;
    default:
	error("approx(): invalid interpolation method");
	break;
    }

    // CODE ALTERATION: permit NA here
#if 0
    for(i = 0 ; i < *nxy ; i++)
	if(ISNA(x[i]) || ISNA(y[i]))
	    error("approx(): attempted to interpolate NA values");
#endif

    M.kind = *method;
    M.ylow = *yleft;
    M.yhigh = *yright;

    // CODE ALTERATION: permit NA in x and y; just make the answer be
    // NA in such cases.
    for(i = 0 ; i < *nout; i++) {
      if (ISNA(x[i]) || ISNA(y[i]) || ISNA(xout[i])) {
	xout[i] = NA_REAL;
      } else {
	xout[i] = approx1(xout[i], x, y, *nxy, &M);
      }
    }
}

//////////////////////////////////////////////////////////////////////

void binmap(int *rule, // 1 or 2, just like for approx()
    double *beamAngle, double *pitch, double *roll, // all of length 1
    int *n, // length of distance, z1-z4, and y1-y4
    double *distance, // like "x"
    double *y1, double *y2, double *y3, double *y4,  // like a set of "y" values
    // below are storage
    double *buffer, // do not allocate locally, for speed
    double *z1, double *z2, double *z3, double *z4,  // calculated here; just supply space
    double *Y1, double *Y2, double *Y3, double *Y4)  // calculated here; just supply space
{
  int i;
  // distance, y, y1-y4 are of length *n
  // beamAngle, pitch, and roll are of length 1
  const double RPD = atan2(1.0, 1.0) / 45.0; // radians/degree
#if 0
  Rprintf("n %d\n", *n);
  Rprintf("beamAngle %f pitch %f roll %f\n", *beamAngle, *pitch, *roll);
#endif
  double cr = cos((*roll) * RPD);
  double sr = sin((*roll) * RPD);
  double cp = cos((*pitch) * RPD);
  double sp = sin((*pitch) * RPD);
  double tt = tan((*beamAngle) * RPD);
#if 0
  Rprintf("C : r %.6f p %.6f cr %.6f sr %.6f cp %.6f sp %.6f tt %.6f\n", *roll, *pitch, cr, sr, cp, sp, tt);
#endif
  for (i=0; i < *n; i++) {
    z1[i] = distance[i] * (cr - tt * sr) * cp;
    z2[i] = distance[i] * (cr + tt * sr) * cp;
    z3[i] = distance[i] * (cp + tt * sp) * cr;
    z4[i] = distance[i] * (cp - tt * sp) * cr;
  }
#if 0
  Rprintf("C : z1       ");
  for (i = 0; i < 8; i++)
    Rprintf("%11.6f ", i, z1[i]);
  Rprintf("\n");
#endif

  // y <- .C(C_R_approx, as.double(x), as.double(y), as.integer(nx), 
  //  xout = as.double(xout), as.integer(length(xout)), as.integer(method), 
  //  as.double(yleft), as.double(yright), as.double(f), NAOK = TRUE, 
  //  PACKAGE = "stats")$xout
  double f = 0.0; // unused, since we set method=1; see docs on approx()
  double left, right;
  // void R_approx(double *x, double *y, int *nxy, double *xout, int *nout,
  //               int *method, double *yleft, double *yright, double *f)
  // NOTE: replaces xout with the interpolated value!
  int method = 1; // "linear" for approx()

  if (*rule == 1) { left = NA_REAL; right = NA_REAL; } else { left = y1[0]; right = y1[*n]; }
  for (i = 0; i < *n; i++) {
    buffer[i] = distance[i];
  }
  R_approx(z1, y1, n, buffer, n, &method, &left, &right, &f);
  for (i = 0; i < *n; i++) {
    Y1[i] = buffer[i];
    buffer[i] = distance[i];
  }

  if (*rule == 1) { left = NA_REAL; right = NA_REAL; } else { left = y2[0]; right = y2[*n]; }
  R_approx(z2, y2, n, buffer, n, &method, &left, &right, &f);
  for (i = 0; i < *n; i++) {
    Y2[i] = buffer[i];
    buffer[i] = distance[i];
  }

  if (*rule == 1) { left = NA_REAL; right = NA_REAL; } else { left = y3[0]; right = y3[*n]; }
  R_approx(z3, y3, n, buffer, n, &method, &left, &right, &f);
  for (i = 0; i < *n; i++) {
    Y3[i] = buffer[i];
    buffer[i] = distance[i];
  }

  if (*rule == 1) { left = NA_REAL; right = NA_REAL; } else { left = y4[0]; right = y4[*n]; }
  R_approx(z4, y4, n, buffer, n, &method, &left, &right, &f);
  for (i = 0; i < *n; i++) {
    Y4[i] = buffer[i];
  }
}
