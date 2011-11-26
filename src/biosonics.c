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

// bytes: bytes holding data
// ns: number of samples, which may exceed length(bytes)/2 because of
//     run-length-encoding
// res: the result
// .Call("biosonic_png", as.raw(c(0x01, 0x02)), 1, res
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
    unsigned short us;
    us = (0xFF00 & bytep[2*i]) | (0x00FF & bytep[1+2*i]);
    unsigned int mantissa = (us & 0x0FFF);
    unsigned int exponent = (us & 0xF000) >> 12;
    if (exponent == 0)
      resp[i] = (float)mantissa;
    else
      resp[i] = (mantissa + 0x1000) << (exponent - 1);
#ifdef DEBUG
    Rprintf("0x%x 0x%x m=%d e=%d %f\n", bytep[2*i], bytep[1+2*i], mantissa, exponent, resp[i]);
#endif
  }
  UNPROTECT(3);
  return(res);
}
//
//DOCS (and note they assemble little-endian):
//These represent A/D converter counts, packed in a custom
//floating-point format with a 4-bit exponent and a 12-bit mantissa.
//Each element x of the array should be decoded as follows to give you
//a ULONG representing counts as a normal integer.
//
//  for (int i = 0; i < *n; i++) {
//   out[i] = (int)b[2*i] + 256 * (int)b[1+2*i];
//  }


