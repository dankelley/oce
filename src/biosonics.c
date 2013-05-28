/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//
// FIXME: this code should be altered to handle dual- and split-beam data.
// In single-beam data, the procedure is to work in 2-byte chunks (as
// is done here) but for the other cases, it should work in 4-byte
// chunks.

// REFERENCES:
//   [1] "DT4 Data File Format Specification" [July, 2010] DT4_format_2010.pdf

// #define DEBUG 1
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

// subsecond time for Biosonic echosounder
// [1 p19]
void biosonics_ss(unsigned char *byte, double *out)
{
  if (!(0x80 & *byte))
    *out = 0.0;
  else
    *out = (float)((int)(0x7F & *byte)) / 100;
}

// SYNOPSIS. handle Biosonics ping data
//
// DETAILS. Convert a 2-byte binary data chunk, into an integer,
// using the  Biosonics-defined floating-point type, which uses
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
//
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
 
// SYNOPSIS. handle Biosonics ping data
//
// DETAILS. Analyse binary data, using different methods for different
// beam types.  The eventual conversion to an integer format
// is done with biosonics_ss().
//  
// ARGUMENTS
//   bytes
//      the data ('raw' in R notation)
//   spp
//       samples per ping (after RLE expansion), i.e. length of return value
//  type
//       0 for single-beam, 1 for split-beam, or 2 for dual-beam
//
// BUGS
//   1. not employing run-length-encoding
//
// RETURN VALUE. numerical values of type double

SEXP biosonics_ping(SEXP bytes, SEXP spp, SEXP type)
{
  PROTECT(bytes = AS_RAW(bytes));
  PROTECT(spp = AS_NUMERIC(spp));
  PROTECT(type = AS_NUMERIC(type));
  double *typep = REAL(type);
  int beamType = (int)floor(0.5 + *typep);
  int datum_length = 2;
  if (beamType == 1) {
    datum_length = 4;
    // error("cannot handle dual-beam echosounder data");
  } else if (beamType == 2) {
    datum_length = 4;
    // error("cannot handle split-beam echosounder data");
  }
  //Rprintf("beamType=%d\n", beamType);
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
  for (int ires = 0; ires < lres; ires++) {
    // zero fill at end, if needed
    if (nbytes <= (2*ires)) {
#ifdef DEBUG
      Rprintf("    padding %d data\n", (2*lres - nbytes)/2);
#endif
      while (ires < lres)
	resp[ires++] = (double)0.0;
      break;
    }
    // The RLE starting code is 0xFF in an odd-numbered byte.
    if (bytep[1 + datum_length * ires] == 0xFF) {
      int zeros = 2 + (int)bytep[datum_length * ires];
#ifdef DEBUG
      Rprintf(" zeros = %d (lres=%d)\n", zeros, lres);
#endif
      if (ires + zeros >= lres)
	zeros = lres - ires;
#ifdef DEBUG
      Rprintf(" AFTER zeros = %d (lres=%d)\n", zeros, lres);
      Rprintf("  > RLE at ires=%d [IGNORED]\n", ires);
      Rprintf("  > n=%d\n", zeros);
#endif
      for (int z = 0; z < zeros; z++) {
	resp[ires + z] = 0.0;
#ifdef DEBUG
	Rprintf("  * %d\n", ires + z);
#endif
      }
      ires += zeros - 1;
    } else {
      // normal 2-byte datum
      resp[ires] = biosonic_float(bytep[datum_length*ires], bytep[1+datum_length*ires]);
    }
  }
  UNPROTECT(4);
  return(res);
}

