/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
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
//   ns
//       number of samples, which may exceed length(bytes)/2 because of
//       run-length-encoding
//
// RETURN VALUE. numerical values (integers)
SEXP biosonics_ping(SEXP bytes, SEXP ns)
{
  PROTECT(bytes = AS_RAW(bytes));
  PROTECT(ns = AS_NUMERIC(ns));
  unsigned char *bytep = RAW(bytes);
  double *nsp = REAL(ns);
  int n = (int)(*nsp);
  SEXP res;
  PROTECT(res = allocVector(REALSXP, n));
  double *resp = REAL(res);
  int nb = LENGTH(bytes);
  // exponent 4 bits, mantissa 12 bits
  for (int i = 0; i < n; i++) {
    if (nb < (2*i)) {
      Rprintf("BUG: not enough bytes to fill out a Biosonics ping vector\n");
      break;
    }
    unsigned short assembled_bytes = (0xFF00 & bytep[2*i]) | (0x00FF & bytep[1+2*i]);
    unsigned int mantissa = (assembled_bytes & 0x0FFF);
    unsigned int exponent = (assembled_bytes & 0xF000) >> 12;
    if (exponent == 0)
      resp[i] = (double)mantissa;
    else
      resp[i] = (double)((mantissa + 0x1000) << (exponent - 1));
#ifdef DEBUG
    Rprintf("   0x%x 0x%x m=%d e=%d %f\n", bytep[2*i], bytep[1+2*i], mantissa, exponent, resp[i]);
#endif
  }
  UNPROTECT(3);
  return(res);
}

