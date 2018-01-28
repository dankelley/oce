/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

// #define DEBUG 1

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#define SAVE(x,y) {\
  if ((*no) >= (*nomax)) error("Ran out of space (limit %d pairs); contact developer.\n", (*nomax));\
  xo[(*no)]=(x);\
  yo[(*no)]=(y);\
  ++(*no);\
}

// smash the opposite side, retaining y but fixing x as x0 +- epsilon
void polygon_subdivide_vertically_smash_1(int *n, double *x, double *y, double *x0,
    int *nomax, int *no, double *xo, double *yo)
{
#ifdef DEBUG
  Rprintf("polygon_subdivide_vertically3(*n=%d, ..., *x0=%f, *nomax=%d, ...)\n", *n, *x0, *nomax);
#endif
  unsigned int *poly_start = (unsigned int*)R_alloc(*(nomax), sizeof(unsigned int));
  unsigned int *poly_end = (unsigned int*)R_alloc(*(nomax), sizeof(unsigned int));
  unsigned int ipoly=0, npoly = 0;
  (*no) = 0; // may be set to 0 by R, but protect against R changes

  // Separate steps to make it easier to write/debug/read.

  // 1. Find polygons.
  // Skip any NA at start
  int start;
  for (start = 0; start < (*n)-1; start++) {
    if (!ISNA(x[start]))
      break;
  }
  poly_start[npoly] = start;
  int i = start;
  while (i < (*n)) {
    // Find first non-NA
    while ((i < (*n)) && ISNA(x[i])) {
      i++;
    }
    poly_start[npoly] = i;
    // Find last non-NA
    while ((i < (*n)) && !ISNA(x[i])) {
      i++;
    }
    poly_end[npoly] = (i == (*n)) ? i - 1 : i;
    npoly++;
    i++;
  }
  if (npoly == 0) 
    error("no polygons\n");
#ifdef DEBUG
  Rprintf("poly_start[0]: %d\n", poly_start[0]);
  Rprintf("poly_end[0]: %d\n", poly_end[0]);
#endif
  //Rprintf("found %d polygons\n", npoly);
  //
  // 2. Process each polygon individually.
  double epsilon = 0.25;
  // FIXME: might help to interpolate in an additional point near the boundary
  // FIXME: the opposite side is ugly but very thin so maybe OK
  for (ipoly = 0; ipoly < npoly; ipoly++) {
    //if (ipoly>280) Rprintf("top ipoly=%d\n", ipoly);
    int crossing = 0;
    double delta0 = x[poly_start[ipoly]] - (*x0);
    if (!delta0) {
      crossing = 1;
    } else {
      //Rprintf(" checking ipoly=%d for a cross\n", ipoly);
      for (i = poly_start[ipoly]; i <= poly_end[ipoly]; i++) {
        double delta = x[i] - (*x0);
        if (delta == 0.0 || delta * delta0 < 0.0) {
          crossing = 1;
          break;
        }
      }
    }
    if (crossing) {
#ifdef DEBUG
      Rprintf("CROSSING\n");
      Rprintf("ipoly=%4d npoly=%d @ i=%d:%d (first y %.1f) CROSSES (recall *n=%d) (no=%d)\n", ipoly, npoly, poly_start[ipoly], poly_end[ipoly], y[poly_start[ipoly]], (*n), *no);
#endif
      for (i = poly_start[ipoly]; i <= poly_end[ipoly]; i++) {
#ifdef DEBUG
        Rprintf(" x[%d]=%.2f y[%d]=%.2f (ipoly=%d) LHS (no=%d)\n", i, x[i], i, y[i], ipoly, *no);
#endif
        if (i == (*n))
          return;
        if (x[i] > ((*x0) - epsilon)) {
          SAVE((*x0) - epsilon, y[i])
        } else {
          SAVE(x[i], y[i])
        }
      }
      SAVE(NA_REAL, NA_REAL);
      for (i = poly_start[ipoly]; i <= poly_end[ipoly]; i++) {
#ifdef DEBUG
        Rprintf(" x[%d]=%.2f y[%d]=%.2f (ipoly=%d) RHS (no=%d)\n", i, x[i], i, y[i], ipoly, *no);
#endif
        if (i == (*n))
          return;
        if (x[i] < ((*x0) + epsilon)) {
          SAVE((*x0) + epsilon, y[i])
        } else {
          SAVE(x[i], y[i])
        }
      }
    } else {
      for (i = poly_start[ipoly]; i <= poly_end[ipoly]; i++) {
#ifdef DEBUG
        Rprintf(" x[%d]=%.2f y[%d]=%.2f (ipoly=%d) NO CROSSING (no=%d)\n", i, x[i], i, y[i], ipoly, *no);
#endif
        if (i < (*n))
          SAVE(x[i], y[i])
      }
      SAVE(NA_REAL, NA_REAL);
    }
  }
}

