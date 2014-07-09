/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

// QUESTION: can this be done in-place?

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG // comment this out when working.

/* 

# Part 1: calculate bytes
system("R CMD SHLIB landsat.c")
dyn.load('landsat.so')
m <- matrix(seq(0, 1, length.out=12), nrow=3, byrow=TRUE)
r1 <- .Call("landsat_numeric_to_bytes", m)


# Part 2: transpose and flip
m <- matrix(as.raw(1:12), nrow=3, byrow=TRUE)
system("R CMD SHLIB landsat.c")
dyn.load('landsat.so')
print("m:")
m
mt <- t(m)
print("t(m):")
mt
r2 <- mt[, seq.int(dim(mt)[2], 1, -1)]
r2[r2==0] <- NA
print("r2 (expected result):")
r2
r1 <- .Call("landsat_transpose_flip", m)
print("r1 (this result):")
r1
stopifnot(all.equal(r1, r2))


*/

// Macros differ because res is transposed so #rows is ncol.
#define ij_m(i, j) ((i) + (nrow) * (j)) // normal indexing (see e.g. gradient.c)
#define ij_res(i, j) ((i) + (nrow_res) * (j)) // transposed indexing

// FIXME: the method is wrong, including macros above and code below

SEXP landsat_transpose_flip(SEXP m)
{
  int nrow = INTEGER(GET_DIM(m))[0];
  int ncol = INTEGER(GET_DIM(m))[1];
  unsigned char *mp = RAW(m); // FIXME: only works for 'raw' matrices
  SEXP res;
  int nrow_res = ncol;
  int ncol_res = nrow;
  PROTECT(res = allocMatrix(RAWSXP, nrow_res, ncol_res));
  unsigned char *resp = RAW(res);
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

SEXP landsat_numeric_to_bytes(SEXP m)
{

  int nrow = INTEGER(GET_DIM(m))[0];
  int ncol = INTEGER(GET_DIM(m))[1];
  Rprintf("landsat_numeric_to_bytes() given matrix with nrow %d and ncol %d\n",
      nrow, ncol);
  SEXP lres;
  SEXP lres_names;
  PROTECT(lres = allocVector(VECSXP, 2));
  PROTECT(lres_names = allocVector(STRSXP, 2));
  SEXP lsb; // least-significant byte matrix
  PROTECT(lsb = allocMatrix(RAWSXP, nrow, ncol));
  SEXP msb; // most-significant byte matrix
  PROTECT(msb = allocMatrix(RAWSXP, nrow, ncol));
  unsigned char* lsbp = RAW_POINTER(lsb);
  unsigned char* msbp = RAW_POINTER(msb);
  // Check endianness
  unsigned int x = 1;
  char *c = (char*) &x;
  int little_endian = (int)*c;
  Rprintf("little_endian: %d\n", little_endian);
  // fill up arrays
  double *mp = REAL(m);
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      double mij = mp[ij_m(i, j)];
      int mij_int = (int)(65535*mij);
      unsigned char ls = mij_int & 0x00FF;
      unsigned char ms = (mij_int & 0xFF00 >> 8);
      Rprintf("i %d, j %d, m: %f -> %d -> %d %d\n", i, j, mij, mij_int, ls, ms);
      lsbp[ij_m(i, j)] = ls;
      msbp[ij_m(i, j)] = ms;
    }
  }
  SET_VECTOR_ELT(lres, 0, lsb);
  SET_STRING_ELT(lres_names, 0, mkChar("lsb"));
  SET_VECTOR_ELT(lres, 1, msb);
  SET_STRING_ELT(lres_names, 1, mkChar("msb"));
  setAttrib(lres, R_NamesSymbol, lres_names);
  UNPROTECT(4);
  return(lres);
}
