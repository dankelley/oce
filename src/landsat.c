/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

// NB. breaking work up into two functions may slow things down but
// it makes it simpler to check.

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

// #define DEBUG // comment this out when working.

/* 

# Part 1: calculate bytes
system("R CMD SHLIB landsat.c")
dyn.load('landsat.so')
m <- matrix(seq(0, 1, length.out=256), nrow=8, byrow=FALSE)
r1 <- .Call("landsat_numeric_to_bytes", m, 8)
r2 <- .Call("landsat_numeric_to_bytes", m, 16)


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

SEXP landsat_numeric_to_bytes(SEXP m, SEXP bits)
{

  int nrow = INTEGER(GET_DIM(m))[0];
  int ncol = INTEGER(GET_DIM(m))[1];
#ifdef DEBUG
  Rprintf("landsat_numeric_to_bytes() given matrix with nrow %d and ncol %d\n",
      nrow, ncol);
#endif
  PROTECT(bits = AS_INTEGER(bits));
  int *bitsp = INTEGER_POINTER(bits);
  int two_byte = (*bitsp) > 8;
#ifdef DEBUG
  Rprintf("landsat_numeric_to_bytes has bits=%d\n", *bitsp);
#endif
  SEXP lres;
  SEXP lres_names;

  SEXP lsb; // least-significant byte matrix
  PROTECT(lsb = allocMatrix(RAWSXP, nrow, ncol));
  unsigned char* lsbp = RAW_POINTER(lsb);
  SEXP msb; // most-significant byte matrix
  if (two_byte)
    PROTECT(msb = allocMatrix(RAWSXP, nrow, ncol));
  else
    PROTECT(msb = allocVector(RAWSXP, 1));
  unsigned char* msbp = RAW_POINTER(msb);
  if (!two_byte)
    *msbp = 0;

  PROTECT(lres = allocVector(VECSXP, 2));
  PROTECT(lres_names = allocVector(STRSXP, 2));

  // Check endianness
  unsigned int x = 1;
  char *c = (char*) &x;
  int little_endian = (int)*c;
#ifdef DEBUG
  Rprintf("little_endian: %d\n", little_endian);
#endif
  // fill up arrays
  double *mp = REAL(m);
  // No need to index by i and j here; this will speed up
  int n = nrow * ncol;
  if (two_byte) {
    if (little_endian) {
      unsigned int mij_int;
      unsigned char ms, ls;
      for (int i = 0; i < n; i++) {
	//double mij = mp[i];
	mij_int = (unsigned int)(65535*mp[i]);
	ms = (mij_int & 0xFF00) >> 8;
	ls = mij_int & 0x00FF;
#ifdef DEBUG
	Rprintf("i %d, m: %f -> %d -> msb 0x%02x lsb 0x%02x (little endian two-byte)\n", i, m[i], mij_int, ms, ls);
#endif
	lsbp[i] = ls;
	msbp[i] = ms;
      }
    } else {
      // big endian below
      for (int i = 0; i < n; i++) {
	double mij = mp[i];
	unsigned int mij_int = (unsigned int)(65535*mij);
	unsigned char ls = (mij_int & 0xFF00) >> 8;
	unsigned char ms = mij_int & 0x00FF;
#ifdef DEBUG
	Rprintf("i %d, m: %f -> %d -> msb 0x%02x lsb 0x%02x (big endian two-byte)\n", i, mij, mij_int, ms, ls);
#endif
	lsbp[i] = ls;
	msbp[i] = ms;
      }
   }
  } else {
    // !two_byte: msb is scalar 0; just fill up lsb
    if (little_endian) {
      for (int i = 0; i < n; i++) {
	double mij = mp[i];
	unsigned int mij_int = (unsigned int)(255*mij);
	unsigned char ls = mij_int; // & 0xFF00;
#ifdef DEBUG
	Rprintf("i=%d   %f -> %d -> lsb 0x%02x (little endian one-byte)\n", i, mij, mij_int, ls);
#endif
	lsbp[i] = ls;
      }
    } else {
      // big endian
      unsigned int mij_int;
      unsigned char ls;
      for (int i = 0; i < n; i++) {
	//double mij = mp[i];
	mij_int = (unsigned int)(255*mp[i]);
	ls = mij_int; // & 0x00FF;
#ifdef DEBUG
	Rprintf("i=%d   %f -> %d -> lsb 0x%02x (big endian one-byte)\n", i, m[i], mij_int, ls);
#endif
	lsbp[i] = ls;
      }
    }
  }
  SET_VECTOR_ELT(lres, 0, lsb);
  SET_STRING_ELT(lres_names, 0, mkChar("lsb"));
  SET_VECTOR_ELT(lres, 1, msb);
  SET_STRING_ELT(lres_names, 1, mkChar("msb"));
  setAttrib(lres, R_NamesSymbol, lres_names);
  UNPROTECT(5);
  return(lres);
}
