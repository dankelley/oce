/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

// Curl calculated with first difference on 5-point stencil.

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG 1

/* 

x <- 1:3
y <- 1:4
mx <- matrix(1:12, nrow=3, byrow=TRUE)
my <- matrix(1:12, nrow=3, byrow=FALSE)
system("R CMD SHLIB curl.c"); dyn.load('curl.so'); curl<-.Call("curl",mx,my,x,y,TRUE)
x
y
curl


*/

// The first macro uses nrow-1 for a centred matrix.
// #define ix2(i, j) ((i) + ((nrow)-1) * (j))
#define ix(i, j) ((i) + (nrow) * (j))

SEXP curl(SEXP mx, SEXP my, SEXP x, SEXP y, SEXP geographical)
{
  double R = 6371.0e3; // FIXME: could be an argument but there seems little need
  PROTECT(mx = AS_NUMERIC(mx));
  PROTECT(my = AS_NUMERIC(my));
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(geographical = AS_NUMERIC(geographical));
  int nrow = INTEGER(GET_DIM(mx))[0];
  if (nrow != INTEGER(GET_DIM(my))[0]) error("matrices mx and my must have nrow");
  int ncol = INTEGER(GET_DIM(mx))[1];
  if (ncol != INTEGER(GET_DIM(my))[1]) error("matrices mx and my must have ncol");
  if (LENGTH(x) != nrow) error("matrix has %d rows, but length(x) is %d", nrow, LENGTH(x));
  if (LENGTH(y) != ncol) error("matrix has %d cols, but length(y) is %d", ncol, LENGTH(y));
  double *mxp = REAL(mx);
  double *myp = REAL(my);
  double *xp = REAL(x);
  double *yp = REAL(y);
  double *geographicalp = REAL(geographical);
  int isGeographical = 0.0 != *geographicalp;
  
  // Construct curl matrix.
  SEXP curl;
  PROTECT(curl = allocMatrix(REALSXP, nrow, ncol));
  double *curlp = REAL(curl);

  // Fill whole grid with NA.  FIXME: special-case the edges instead.
  // SCHEME: x or longitude is indexed by 0 <= i <= (nrow-1), while
  //         y or latitude  is indexed by 0 <= j <= (ncol-1).
  for (int i = 0; i < nrow; i++) 
    for (int j = 0; j < ncol; j++) 
      curlp[ix(i,j)] = NA_REAL; 
  double xfac=1.0, yfac = 1.0;
  if (isGeographical)
    yfac = R * M_PI / 180.0;
  for (int j = 1; j < ncol-1; j++) {
    if (isGeographical)
      xfac = R * M_PI / 180.0 * cos(0.5*yp[j]*M_PI/180.0);
    for (int i = 1; i < nrow-1; i++) {
      // Calculate first difference with a 5-point stencil, e.g. infer d/dy by subtracting 
      // the value at i+1 from the value at i-1 etc.
      double dmxdy = (mxp[ix(i,j+1)] - mxp[ix(i,j-1)]) / (yp[j+1] - yp[j-1]) / xfac;
      double dmydx = (myp[ix(i+1,j)] - myp[ix(i-1,j)]) / (xp[i+1] - xp[i-1]) / yfac;
      curlp[ix(i, j)] = dmydx - dmxdy;
#ifdef DEBUG
      Rprintf("x[%d,%d]=(%.1f,%.1f), y[%d,%d]=(%.1f,%.1f)\n", j-1, j+1, xp[j-1], xp[j+1], i-1,i+1,yp[i-1], yp[i+1]);
      Rprintf("  dmydx=%.2e; dmxdy=%.2e; curl=%.1e\n", dmydx, dmxdy, curlp[ix(i,j)]);
#endif
    }
  }

  // Construct list for the return value.
  SEXP lres;
  SEXP lres_names;
  PROTECT(lres = allocVector(VECSXP, 3));
  PROTECT(lres_names = allocVector(STRSXP, 3));
  SET_VECTOR_ELT(lres, 0, x);
  SET_VECTOR_ELT(lres, 1, y);
  SET_STRING_ELT(lres_names, 0, mkChar("x"));
  SET_STRING_ELT(lres_names, 1, mkChar("y"));
  SET_VECTOR_ELT(lres, 2, curl);
  SET_STRING_ELT(lres_names, 2, mkChar("curl"));
  setAttrib(lres, R_NamesSymbol, lres_names);
  UNPROTECT(8);
  return(lres);
}

