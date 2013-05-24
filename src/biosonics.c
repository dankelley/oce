/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
// #define DEBUG 1
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

// subsecond time for Biosonic echosounder
// REF: p19 of Biosonics "DT4 Data File Format Specification" [July, 2010]
void biosonics_ss(unsigned char *byte, double *out)
{
  if (!(0x80 & *byte))
    *out = 0.0;
  else
    *out = (float)((int)(0x7F & *byte)) / 100;
}

// SYNOPSIS. handle Biosonics ping data
//
// DETAILS.  Assemble bytes into integers, using the
// Biosonics-defined floating-point type, which uses
// pairs of bytes, from which we extract an
// 4-bit exponent and a 12-bit mantissa. 
//  
// ARGUMENTS
//   bytes
//      the data ('raw' in R notation)
//   spp
//       samples per ping (after RLE expansion), i.e. length of return value
//
// BUGS
//   1. not employing run-length-encoding
//
// RETURN VALUE. numerical values of type double
double biosonic_float(unsigned char byte1, unsigned char byte2)
{
    unsigned int assembled_bytes = ((short)byte2 << 8) | ((short)byte1); // little endian
    unsigned int mantissa = (assembled_bytes & 0x0FFF); // rightmost 12 bits (again, little endian)
    int exponent = (assembled_bytes & 0xF000) >> 12; // leftmost 4 bits, shifted to RHS
    unsigned long res;
    if (exponent == 0) {
      res = mantissa;
    } else {
      res = (mantissa + 0x1000) << (exponent - 1);
    }
#ifdef DEBUG
//    Rprintf(" ***  0x%x%x mantissa=%d exponent=%d res=%f\n", byte1, byte2, mantissa, exponent, resp[i]);
#endif
    return((double)res);
}
 

//SEXP biosonics_ping_dual_beam(SEXP bytes, SEXP spp)
//SEXP biosonics_ping_split_beam(SEXP bytes, SEXP spp)

SEXP biosonics_ping_single_beam(SEXP bytes, SEXP spp)
{
  PROTECT(bytes = AS_RAW(bytes));
  PROTECT(spp = AS_NUMERIC(spp));
  unsigned int nbytes = LENGTH(bytes);
  unsigned char *bytep = RAW(bytes);
  double *sppPtr = REAL(spp);
  int lres = (int)(*sppPtr);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, lres));
#ifdef DEBUG
      Rprintf("allocVector(REALSXP, %d)\n", lres);
#endif
  double *resp = REAL(res);
  for (int i = 0; i < lres; i++) {
    // zero fill at end, if needed
    if (nbytes <= (2*i)) {
#ifdef DEBUG
      Rprintf("    padding %d data\n", (2*lres - nbytes)/2);
#endif
      while (i < lres)
	resp[i++] = (double)0.0;
      break;
    }
    // The RLE starting code is 0xFF in an odd-numbered byte.
    if (bytep[1+2*i] == 0xFF) {
      int zeros = 2 + (int)bytep[2*i];
#ifdef DEBUG
      Rprintf(" zeros = %d (lres=%d)\n", zeros, lres);
#endif
      if (i + zeros >= lres)
	zeros = lres - i;
#ifdef DEBUG
      Rprintf(" AFTER zeros = %d (lres=%d)\n", zeros, lres);
      Rprintf("  > RLE at i=%d [IGNORED]\n", i);
      Rprintf("  > n=%d\n", zeros);
#endif
      for (int z = 0; z < zeros; z++) {
	resp[i + z] = 0.0;
#ifdef DEBUG
	Rprintf("  * %d\n", i + z);
#endif
      }
      i += zeros - 1;
    } else {
      // normal 2-byte datum
      resp[i] = biosonic_float(bytep[2*i], bytep[1+2*i]);
    }
  }
  UNPROTECT(3);
  return(res);
}

