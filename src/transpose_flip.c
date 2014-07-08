/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

// QUESTION: can this be done in-place?

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
 
m <- 0.0 + matrix(1:12, nrow=3, byrow=TRUE)
system("R CMD SHLIB transpose_flip.c")
dyn.load('transpose_flip.so')
m2 <- .Call("transpose_flip",m, package="oce")
mt <- t(m)
m3 <- mt[, seq.int(dim(mt)[2], 1, -1)]
m3[m3==0] <- NA
print("m:")
m
print("mt:")
mt
print("m2:")
m2
print("m3:")
m3
*/

// the macros differ because res is transposed so #rows is ncol.
#define ijm(i, j) ((i) + (nrow) * (j)) // normal indexing (see e.g. gradient.c)
#define ijres(i, j) ((i) + (ncol) * (j)) // transposed indexing

// FIXME: the method is wrong, including macros above and code below

SEXP transpose_flip(SEXP m)
{
  int nrow = INTEGER(GET_DIM(m))[0];
  int ncol = INTEGER(GET_DIM(m))[1];
  double *mp = REAL(m); // FIXME: only works for real matrices

  SEXP res;
  PROTECT(res = allocMatrix(REALSXP, ncol, nrow)); // notice row-col flip
  double *resp = REAL(res);

  // Transpose
  Rprintf("transpose:\n");
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      Rprintf("i %d, j %d, ijres(i,j) %d, ijm(j, i) %d\n", i, j, ijres(i,j), ijm(j,i));
      resp[ijres(j, i)] = mp[ijm(i, j)];
    }
  }
  // Flip in second dimension
  Rprintf("flip:\n");
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      Rprintf("i %d, j %d, ijres(i,j) %d, ijres(i,ncol-j-1) %d\n",
	  i, j, ijres(i,j), ijres(i, ncol-j-1));
      double tmp = resp[ijres(i, j)];
      resp[ijres(i, j)] = resp[ijres(i, ncol-j-1)];
      resp[ijres(i, ncol-j-1)] = tmp;
    }
  }
  Rprintf("FLIP NOT WORKING\n");
  UNPROTECT(1);
  return(res);
}

