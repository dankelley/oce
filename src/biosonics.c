/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//#define DEBUG 1
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
// pairs of bytes, assembled in order to get
// a 4-bit exponent followed by 12-bit mantissa. 
//  
// NOTE. tests show that the bytes are assembled in
// order, which is a bit of a surprise since the 
// Biosonics documents suggest a little-endian
// scheme.
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
// RETURN VALUE. numerical values (integers)
//
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
    Rprintf(" ***  0x%x%x mantissa=%d exponent=%d res=%f\n", bytep[2*i], bytep[1+2*i], mantissa, exponent, resp[i]);
#endif
    return((double)res);
}
 

SEXP biosonics_ping(SEXP bytes, SEXP spp)
{
  PROTECT(bytes = AS_RAW(bytes));
  PROTECT(spp = AS_NUMERIC(spp));
  unsigned int nbytes = LENGTH(bytes);
  unsigned char *bytep = RAW(bytes);
  double *sppPtr = REAL(spp);
  int lres = (int)(*sppPtr);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, lres));
  double *resp = REAL(res);
  for (int i = 0; i < lres; i++) {
    if (nbytes <= (2*i)) { // zero fill at end, if needed (should not be)
#ifdef DEBUG
      Rprintf("    padding %d data\n", 2*lres - nbytes);
#endif
      while (i < lres)
	resp[i++] = (double)0.0;
      break;
    }
    // RLE must start with odd-numbered (even-numbered??) byte
    if (bytep[1+2*i] == 0xFF) {
#ifdef DEBUG
      Rprintf("  > RLE at i=%d [IGNORED]\n", i);
      Rprintf("  > n=%d\n", 1+(int)bytep[2*i]);
#endif
    }
    resp[i] = biosonic_float(bytep[2*i], bytep[1+2*i]);
  }
  UNPROTECT(3);
  return(res);
}

