/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//
// FIXME: this code should be altered to handle dual- and split-beam data.
// In single-beam data, the procedure is to work in 2-byte chunks (as
// is done here) but for the other cases, it should work in 4-byte
// chunks.

// REFERENCES:
//   [1] "DT4 Data File Format Specification" [July, 2010] DT4_format_2010.pdf

#define DEBUG 1
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
    
static unsigned char *buffer1 = NULL;
static unsigned int *buffer4 = NULL;

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
//#ifdef DEBUG
//    Rprintf(" ***  0x%x%x mantissa=%d exponent=%d res=%f\n", byte1, byte2, mantissa, exponent, resp[i]);
//#endif
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
#ifdef DEBUG
  Rprintf("biosonics_ping(bytes, spp, type) decoded beamType=%d\n", beamType);
#endif
  int datum_length = 2;
  if (beamType == 1 || beamType == 2) {
    datum_length = 4;
  }
  unsigned int nbytes = LENGTH(bytes);
  unsigned char *bytep = RAW(bytes);
  unsigned int *uip = (unsigned int*)RAW(bytes);
  //Rprintf("unsigned int length %d; unsigned long int length %d; ptr %ld %ld\n", sizeof(unsigned int), sizeof(unsigned long int), bytep, uip);
  //Rprintf("nbytes %d\n", nbytes);
  double *sppPtr = REAL(spp);
  int lres = (int)(*sppPtr);
  SEXP res;
  SEXP res_names;
  PROTECT(res = allocVector(VECSXP, 2));
  PROTECT(res_names = allocVector(STRSXP, 2));
  SEXP res_a;
  PROTECT(res_a = allocVector(REALSXP, lres));
  SEXP res_b;
  PROTECT(res_b = allocVector(REALSXP, lres));
  // Expand unlength-encoded data
  if (beamType == 0) {
    if (buffer1 == NULL) {
#ifdef DEBUG
      Rprintf("should allocate space for %d data (1byte) now\n", lres);
#endif
      buffer1 = (unsigned char*) calloc(lres, sizeof(unsigned char));
      if (buffer1 == NULL) {
	error("cannot allocate space for temporary buffer (named buffer1) of raw bytes, of length %d", lres);
      }
#ifdef DEBUG
      Rprintf("... allocation was OK; address is %lx\n", buffer1);
#endif
    } else {
#ifdef DEBUG
      Rprintf("using existing buffer1 at address %lx\n", buffer1);
#endif
    }
  }
  if (beamType == 1 || beamType == 2) {
    if (buffer4 == NULL) {
#ifdef DEBUG
      Rprintf("should allocate space for %d data (4byte) now\n", lres);
#endif
      buffer4 = (unsigned int*) calloc(lres, sizeof(unsigned int));
      if (buffer4 == NULL) {
	error("cannot allocate space for temporary buffer (named buffer4) of 4-bytes chunks, of length %d", lres);
      }
#ifdef DEBUG
      Rprintf("... allocation was OK; address is %lx\n", buffer4);
#endif
    } else {
#ifdef DEBUG
      Rprintf("using existing buffer4 at address %lx\n", buffer4);
#endif
    }
  } 
  if (beamType == -1) {
#ifdef DEBUG
    Rprintf("should clear space\n", lres);
#endif
    if (buffer1 != NULL)
      free(buffer1);
    if (buffer4 != NULL)
      free(buffer4);
    buffer1 = NULL;
    buffer4 = NULL;
    UNPROTECT(4);
    return(res);
  }
#ifdef DEBUG
  Rprintf("allocVector(REALSXP, %d)\n", lres);
#endif
  double *resap = REAL(res_a);
  double *resbp = REAL(res_b);
  for (int dan = 0; dan < nbytes / 4; dan++) {
#ifdef DEBUG
    Rprintf(" %lx ", (unsigned int)uip[dan]);
#endif
    //if ((uip[dan] & 0xFF000000) == 0xFF000000) {
    //  Rprintf(" A. 4byte match to FF000000 at 4byte index %d or 1byte index %d\n", dan, 4*dan);
    //}
    //if ((uip[dan] & 0x00FF0000) == 0x00FF0000) {
    //  Rprintf(" B. 4byte match to 00FF0000 at 4byte index %d or 1byte index %d\n", dan, 4*dan);
    //}
    if ((uip[dan] & 0x0000FF00) == 0x0000FF00) {
      int n = (uip[dan] & 0x000000FF) + 2;
#ifdef DEBUG
      Rprintf(" 4byte match of %lx to 0000FF00 at 4byte index %d or 1byte index %d; n=%d\n",
	  (unsigned int)uip[dan], dan, 4*dan, n);
#endif
    }
    //if ((uip[dan] & 0x000000FF) == 0x000000FF) {
    //  Rprintf(" D. 4byte match to 000000FF at 4byte index %d or 1byte index %d\n", dan, 4*dan);
    //}
  }
  for (int ires = 0; ires < lres; ires++) {
    // zero fill at end, if needed
    if (nbytes <= (2*ires)) {
#ifdef DEBUG
      Rprintf("    padding %d data\n", (2*lres - nbytes)/2);
#endif
      while (ires < lres) {
	resap[ires] = (double)0.0;
	resbp[ires] = (double)0.0;
	ires++;
      }
      break;
    }
    // The RLE starting code is 0xFF in an odd-numbered byte.
    // FIXME: does this work for 4byte data?
    if (bytep[1 + datum_length * ires] == 0xFF) {
      int zeros = 2 + (int)bytep[datum_length * ires];
#ifdef DEBUG
      Rprintf("RLE ires: %d, zeros: %d, lres: %d, flag-at: %d\n", ires, zeros, lres, 1+datum_length*ires);
#endif
      if (ires + zeros >= lres)
	zeros = lres - ires;
      for (int z = 0; z < zeros; z++) {
	resap[ires] = 0.0;
	resbp[ires] = 0.0;
	ires++;
      }
    } else {
      // normal 2-byte datum
      resap[ires] = biosonic_float(bytep[datum_length*ires], bytep[1+datum_length*ires]);
      resbp[ires] = biosonic_float(bytep[datum_length*(1+ires)], bytep[1+datum_length*(1+ires)]);
    }
  }
  SET_VECTOR_ELT(res, 0, res_a);
  SET_VECTOR_ELT(res, 1, res_b);
  SET_STRING_ELT(res_names, 0, mkChar("a"));
  SET_STRING_ELT(res_names, 1, mkChar("b"));
  setAttrib(res, R_NamesSymbol, res_names);
  UNPROTECT(7);
  return(res);
}

