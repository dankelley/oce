/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

// QUESTION: can this be done in-place?

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG // comment this out when working.

/* 


m <- 0.0 + matrix(1:12, nrow=3, byrow=TRUE)
system("R CMD SHLIB transpose_flip.c")
dyn.load('transpose_flip.so')
print("m:")
m
mt <- t(m)
print("t(m):")
mt
r2 <- mt[, seq.int(dim(mt)[2], 1, -1)]
r2[r2==0] <- NA
print("r2 (expected result):")
r2
r1 <- .Call("transpose_flip", m)
print("r1 (this result):")
r1


*/

// Macros differ because res is transposed so #rows is ncol.
#define ij_m(i, j) ((i) + (nrow) * (j)) // normal indexing (see e.g. gradient.c)
#define ij_res(i, j) ((i) + (nrow_res) * (j)) // transposed indexing

// FIXME: the method is wrong, including macros above and code below

SEXP transpose_flip(SEXP m)
{
  int nrow = INTEGER(GET_DIM(m))[0];
  int ncol = INTEGER(GET_DIM(m))[1];
  double *mp = REAL(m); // FIXME: only works for real matrices

  SEXP res;
  int nrow_res = ncol;
  int ncol_res = nrow;
  PROTECT(res = allocMatrix(REALSXP, nrow_res, ncol_res));
  double *resp = REAL(res);

  // Transpose
#ifdef DEBUG
  Rprintf("transpose:\n");
#endif
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
#ifdef DEBUG
      Rprintf("i %d, j %d, ij_res(i,j) %d, ij_m(j, i) %d\n",
	  i, j, ij_res(i,j), ij_m(j,i));
#endif
      resp[ij_res(j, i)] = mp[ij_m(i, j)];
    }
  }
  // Flip in second dimension
#ifdef DEBUG
  Rprintf("flip:\n");
#endif
  // ncol_res_half is to avoid undoing the flip by repeating it.
  int ncol_res_half = (int)floor(ncol_res / 2.0);
  for (int i = 0; i < nrow_res; i++) {
    for (int j = 0; j < ncol_res_half; j++) {
#ifdef DEBUG
      Rprintf("i %d, j %d, ncol_res-j-1 %d, ij_res(i,j) %d, ij_res(i,ncol_res-j-1) %d (%f -> %f)\n",
	  i, j, ncol_res-j-1,
	  ij_res(i,j), ij_res(i, ncol_res-j-1),
	  resp[ij_res(i, j)], resp[ij_res(i,ncol_res-j-1)]);
#endif
      double tmp = resp[ij_res(i, j)];
      resp[ij_res(i, j)] = resp[ij_res(i, ncol_res-j-1)];
      resp[ij_res(i, ncol_res-j-1)] = tmp;
    }
  }
  UNPROTECT(1);
  return(res);
}

