/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//
// Try to average across bands; where both bands have any of the
// following codes, return 0xff which will be coded as land in 
// plot.amsr().
//
//     0xff # land mass
//     0xfe # no observations
//     0xfd # bad observations
//     0xfc # sea ice
//     0xfb # missing SST or wind due to rain, or missing water vapour due to heavy rain
//
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/*

   system("R CMD shlib amsr.c")
   dyn.load("amsr.so")
   a <- as.raw(1:7)
   b <- as.raw(7:13)
   a[1] <- as.raw(0xff)
   a[4] <- as.raw(0xfb)
   a[5] <- as.raw(0xfb)
   b[1] <- as.raw(0xff)
   a[2] <- as.raw(0xff)
   b[3] <- as.raw(0xff)
   b[4] <- as.raw(0xfb)
   b[6] <- as.raw(0xfb)
   ab <- .Call("amsr_average", a, b)
   stopifnot(all.equal(ab, as.raw(c(0xff, 0xff, 0xff, 0xfb, 0x0b, 0x06, 0x0a))))

 */

SEXP amsr_average(SEXP a, SEXP b)
{
  PROTECT(a = AS_RAW(a));
  PROTECT(b = AS_RAW(b));
  int na = LENGTH(a), nb=LENGTH(b);
  if (na != nb)
      error("lengths must agree but length(a) is %d and length(b) is %d", na, nb);
  unsigned char *ap = RAW_POINTER(a);
  unsigned char *bp = RAW_POINTER(b);
  SEXP res;
  PROTECT(res = NEW_RAW(na));
  unsigned char *resp = RAW_POINTER(res);
  unsigned char A, B;
  for (int i = 0; i < na; i++) {
    A = ap[i];
    B = bp[i];
    if (A < 0xfb && B < 0xfb) { // A and B are both OK (the most common case, so put first here)
      resp[i] = (unsigned char)(0.5+0.5*(A+B)); // note rounding

    } else if (A == 0xff) { // A is land; ignore B and return code for land
      resp[i] = 0xff;
    } else if (B == 0xff) { // B is land; ignore A and return code for land
      resp[i] = 0xff;

    } else if (A == 0xfe) { // 254
      resp[i] = B; // no A observation, so use B, whatever it is
    } else if (B == 0xfe) {
      resp[i] = A; // no B observation, so use A, whatever it is

    } else if (A == 0xfd) { // 253
      resp[i] = B; // bad A observation, so use B, whatever it is
    } else if (B == 0xfd) {
      resp[i] = A; // bad B observation, so use A, whatever it is

    } else if (A == 0xfc) { // 252
      resp[i] = B; // A had sea ice; try B (although it is likely also ice)
    } else if (B == 0xfc) {
      resp[i] = A; // A had sea ice; try A (although it is likely also ice)

    } else if (A == 0xfb) { // 251
      resp[i] = B; // A was too rainy; try B, on the hope that rain is short-lived
    } else if (B == 0xfb) {
      resp[i] = A; // B was too rainy; try A, on the hope that rain is short-lived

    } else {
      resp[i] = 0xff; // Cannot get here
    }
  }
  UNPROTECT(3);
  return(res);
}

// R-exts.pdf sec 5.9.6, page 117
SEXP getListElement(SEXP list, const char *str)
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); i++) {
    if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  }
  return elmt;
}

/*
 
   system("R CMD shlib amsr.c")
   dyn.load("amsr.so")
   a <- array(as.raw(0:255), dim=c(20,20,3))
   A <- .Call("amsr_composite", a)

*/


// a is an array with e.g. a[,,1] being a matrix of data in the first image
SEXP amsr_composite(SEXP a)
{
  Rprintf("amsr_composite ...\n");
  PROTECT(a = AS_RAW(a));
  unsigned char *ap = RAW_POINTER(a);
  unsigned int n1 = INTEGER(GET_DIM(a))[0];
  unsigned int n2 = INTEGER(GET_DIM(a))[1];
  unsigned int n3 = INTEGER(GET_DIM(a))[2];
  unsigned int n12 = n1 * n2;
  Rprintf("amsr_composite n1=%d n2=%d n3=%d n12=%d\n", n1, n2, n3, n12);
  SEXP res;
  PROTECT(res = NEW_RAW(n12));
  unsigned char *resp = RAW_POINTER(res);
  unsigned char A;
  for (int i = 0; i < n12; i++) {
    double sum = 0.0;
    int nsum = 0;
    if (i < 300) Rprintf("i=%d:\n", i);
    for (int i3 = 0; i3 < n3; i3++) {
      A = ap[i + n12*i3];
      if (A < 0xfb) {
	sum += A;
	nsum++;
	if (i < 300) Rprintf("    i3=%3d A=%3d=0x%02x sum=%5.1f nsum=%d\n", i3, (int)A, A, sum, nsum);
      } else {
	if (i < 300) Rprintf("    i3=%3d A=%3d=0x%02x SKIPPED\n", i3, (int)A, A);
      }
    }
    if (nsum)
      resp[i] = (unsigned char)floor(0.5 + sum/nsum);
    else
      resp[i] = A; // will be >= 0xfb ... we inherit the NA type from last image
    if (i < 300) Rprintf("    resp=%d=0x%02x\n", (int)resp[i], resp[i]);
  }
  UNPROTECT(2);
  return res;
}

