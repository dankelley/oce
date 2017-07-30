/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

// Curl calculated with either of two methods, the
// difference being how derivatives are estimated.

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG 1

/* 

x <- 1:3
y <- 1:4
u <- matrix(1:12, nrow=3, byrow=TRUE)
v <- matrix(1:12, nrow=3, byrow=FALSE)
system("R CMD SHLIB curl.c")
dyn.load('curl.so')
.Call("curl1",u,v,x,y,TRUE)


lat <- c(85,82.5,80,77.5)
lon <- c(160,162.5,165,167.5)
taux <- matrix(c(1,6,9,3,7,11,14,10,4,13,2,12,16,8,15,5), nrow=4, ncol=4, byrow=TRUE)
tauy <- matrix(c(3,20,1,14,1,19,2,6,21,13,28,16,24,4,15,17), nrow=4, ncol=4, byrow=TRUE)
system("R CMD SHLIB curl.c")
dyn.load('curl.so')
.Call("curl2",taux,tauy,lon,lat,TRUE)


*/

SEXP curl1(SEXP u, SEXP v, SEXP x, SEXP y, SEXP geographical)
{
#define ij(i, j) ((i) + (nrow) * (j))
  double R = 6371.0e3; // FIXME: could be an argument but there seems little need
  PROTECT(u = AS_NUMERIC(u));
  PROTECT(v = AS_NUMERIC(v));
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(geographical = AS_NUMERIC(geographical));
  int nrow = INTEGER(GET_DIM(u))[0];
  if (nrow != INTEGER(GET_DIM(v))[0]) error("matrices u and v must have nrow");
  int ncol = INTEGER(GET_DIM(u))[1];
  if (ncol != INTEGER(GET_DIM(v))[1]) error("matrices u and v must have ncol");
  if (LENGTH(x) != nrow) error("matrix has %d rows, but length(x) is %d", nrow, LENGTH(x));
  if (LENGTH(y) != ncol) error("matrix has %d cols, but length(y) is %d", ncol, LENGTH(y));
  double *up = REAL(u);
  double *vp = REAL(v);
  double *xp = REAL(x);
  double *yp = REAL(y);
  double *geographicalp = REAL(geographical);
  int isGeographical = 0.0 != *geographicalp;
  // Construct curl matrix.
  SEXP curl;
  PROTECT(curl = allocMatrix(REALSXP, nrow, ncol));
  double *curlp = REAL(curl);
  // SCHEME: x or longitude is indexed by 0 <= i <= (nrow-1), while
  //         y or latitude  is indexed by 0 <= j <= (ncol-1).
  // set to NA so we can see if we are failing to fill grid correctly
  for (int i = 0; i < nrow; i++) 
    for (int j = 0; j < ncol; j++) 
      curlp[ij(i,j)] = NA_REAL; 
  double xfac=1.0, yfac = 1.0;
  if (isGeographical)
    yfac = R * M_PI / 180.0;
  for (int j = 1; j < ncol-1; j++) {
    if (isGeographical)
      xfac = yfac * cos(yp[j]*M_PI/180.0);
    for (int i = 1; i < nrow-1; i++) {
      // Calculate first difference with a 5-point stencil, e.g. infer d/dx by subtracting 
      // the value at i-1 from the value at i+1 etc.
      double du = up[ij(i,j+1)] - up[ij(i,j-1)];
      double dv = vp[ij(i+1,j)] - vp[ij(i-1,j)];
      double dx = xfac * (xp[i+1] - xp[i-1]);
      double dy = yfac * (yp[j+1] - yp[j-1]);
      curlp[ij(i, j)] = dv/dx - du/dy;
    }
  }
  // bottom and top: copy neighbours above and below
  for (int i = 1; i < nrow-1; i++) {
      curlp[ij(i, 0)] = curlp[ij(i, 1)];
      curlp[ij(i, ncol-1)] = curlp[ij(i, ncol-2)];
  }
  // left and right: copy neighbors to right and left
  for (int j = 1; j < ncol-1; j++) {
      curlp[ij(0, j)] = curlp[ij(1, j)];
      curlp[ij(nrow-1, j)] = curlp[ij(nrow-2, j)];
  }
  // corners: use diagonal neighbour
  curlp[ij(0,0)] = curlp[ij(1,1)];
  curlp[ij(0,ncol-1)] = curlp[ij(1,ncol-2)];
  curlp[ij(nrow-1,0)] = curlp[ij(nrow-2,1)];
  curlp[ij(nrow-1,ncol-1)] = curlp[ij(nrow-2,ncol-2)];
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
#undef ij
}

SEXP curl2(SEXP u, SEXP v, SEXP x, SEXP y, SEXP geographical)
{
  // ij() is in original space, while IJ() is in the smaller space
  // that comes from first differencing.
#define ij(i, j) ((i) + ((nrow)  ) * (j))
#define IJ(i, j) ((i) + ((nrow)-1) * (j))
  double R = 6371.0e3; // FIXME: could be an argument but there seems little need
  PROTECT(u = AS_NUMERIC(u));
  PROTECT(v = AS_NUMERIC(v));
  PROTECT(x = AS_NUMERIC(x));
  PROTECT(y = AS_NUMERIC(y));
  PROTECT(geographical = AS_NUMERIC(geographical));
  int nrow = INTEGER(GET_DIM(u))[0];
  if (nrow != INTEGER(GET_DIM(v))[0]) error("matrices u and v must have nrow");
  int ncol = INTEGER(GET_DIM(u))[1];
  if (ncol != INTEGER(GET_DIM(v))[1]) error("matrices u and v must have ncol");
  if (LENGTH(x) != nrow) error("matrix has %d rows, but length(x) is %d", nrow, LENGTH(x));
  if (LENGTH(y) != ncol) error("matrix has %d cols, but length(y) is %d", ncol, LENGTH(y));
  double *up = REAL(u);
  double *vp = REAL(v);
  double *xp = REAL(x);
  double *yp = REAL(y);
  double *geographicalp = REAL(geographical);
  int isGeographical = 0.0 != *geographicalp;
  // Construct curl matrix.
  SEXP curl;
  PROTECT(curl = allocMatrix(REALSXP, nrow-1, ncol-1));
  double *curlp = REAL(curl);
  // SCHEME: x or longitude is indexed by 0 <= i <= (nrow-1), while
  //         y or latitude  is indexed by 0 <= j <= (ncol-1).
  // set to NA so we can see if we are failing to fill grid correctly
  for (int i = 0; i < nrow-1; i++) 
    for (int j = 0; j < ncol-1; j++) 
      curlp[IJ(i,j)] = NA_REAL; 
  double xfac=1.0, yfac = 1.0;
  if (isGeographical)
    yfac = R * M_PI / 180.0;
#ifdef DEBUG
  Rprintf("nrow= %d\n", nrow);
  Rprintf("ncol= %d\n", ncol);
  Rprintf("2.5*yfac=%.5e\n", 2.5*yfac);
#endif
  for (int j = 0; j < ncol-1; j++) {
    // For xfac, use cosine factor as the average of values at the north
    // and south sides of the grid box.
    if (isGeographical)
      xfac = yfac * 0.5*(cos(yp[j]*M_PI/180.0)+cos(yp[j+1]*M_PI/180.0));
#ifdef DEBUG
    Rprintf("at j=%d, have yp[%d]=%.5f and 2.5*xfac=%.5e\n", j  , j  , yp[j  ], 2.5*yfac * cos(yp[j  ]*M_PI/180.0));
    Rprintf("at j=%d, have yp[%d]=%.5f and 2.5*xfac=%.5e\n", j+1, j+1, yp[j+1], 2.5*yfac * cos(yp[j+1]*M_PI/180.0));
#endif
    for (int i = 0; i < nrow-1; i++) {
      // Calculate derivatives centred within each grid box. Thus, for
      // du, we must avg across x (i.e i) at two grid points, and then
      // difference across y (i.e. j).
      double du = 0.5*(up[ij(i  , j+1)] + up[ij(i+1, j+1)]) - 0.5*(up[ij(i  , j  )] + up[ij(i+1, j  )]);
      double dv = 0.5*(vp[ij(i+1, j  )] + vp[ij(i+1, j+1)]) - 0.5*(vp[ij(i  , j  )] + vp[ij(i  , j+1)]);
      double dx = xfac * (xp[i+1] - xp[i]);
      double dy = yfac * (yp[j+1] - yp[j]);
      curlp[IJ(i, j)] = dv/dx - du/dy;
#ifdef DEBUG
      if (i == 0 && j == 0) {
        Rprintf("du = 0.5*(%g + %g) - 0.5*(%g + %g)\n", up[ij(i,j+1)],up[ij(i+1,j+1)],up[ij(i,j)],up[ij(i+1,j)]);
        Rprintf("dv = 0.5*(%g + %g) - 0.5*(%g + %g)\n", vp[ij(i+1,j)],vp[ij(i+1,j+1)],vp[ij(i,j)],vp[ij(i,j+1)]);
        Rprintf("TEST INDEXING\n");
        Rprintf(" u[0,0] = u[%d] = %g\n", ij(0, 0), up[ij(0, 0)]);
        Rprintf(" u[0,1] = u[%d] = %g\n", ij(0, 1), up[ij(0, 1)]);
        Rprintf(" u[1,0] = u[%d] = %g\n", ij(1, 0), up[ij(1, 0)]);
        Rprintf(" u[1,1] = u[%d] = %g\n", ij(1, 1), up[ij(1, 1)]);
        Rprintf("x[i=%d,%d]=(%.1f,%.1f), y[j=%d,%d]=(%.1f,%.1f)\n", i, i+1, xp[i], xp[i+1], j,j+1,yp[j], yp[j+1]);
        Rprintf("du=%.4f dv=%.4f dx=%g dy=%g dv/dx=%.3e du/dy=%.3e curl=%.3e\n",
            du,dv,dx,dy,dv/dx,du/dy,curlp[ij(i,j)]);
      }
#endif
    }
  }
  // Construct xnew and ynew, which we will return.
  SEXP xnew, ynew;
  PROTECT(xnew = allocVector(REALSXP, nrow-1));
  PROTECT(ynew = allocVector(REALSXP, ncol-1));
  double *xnewp = REAL(xnew);
  double *ynewp = REAL(ynew);
  for (int i = 0; i < nrow - 1; i++)
    xnewp[i] = 0.5 * (xp[i] + xp[i+1]);
  for (int j = 0; j < ncol - 1; j++)
    ynewp[j] = 0.5 * (yp[j] + yp[j+1]);
  // Construct list for the return value.
  SEXP lres;
  SEXP lres_names;
  PROTECT(lres = allocVector(VECSXP, 3));
  PROTECT(lres_names = allocVector(STRSXP, 3));
  SET_VECTOR_ELT(lres, 0, xnew);
  SET_VECTOR_ELT(lres, 1, ynew);
  SET_VECTOR_ELT(lres, 2, curl);
  SET_STRING_ELT(lres_names, 0, mkChar("x"));
  SET_STRING_ELT(lres_names, 1, mkChar("y"));
  SET_STRING_ELT(lres_names, 2, mkChar("curl"));
  setAttrib(lres, R_NamesSymbol, lres_names);
  UNPROTECT(10);
  return(lres);
#undef ij
#undef IJ
}

