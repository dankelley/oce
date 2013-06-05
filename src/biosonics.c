/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
//
// FIXME: this code should be altered to handle dual- and split-beam data.
// In single-beam data, the procedure is to work in 2-byte chunks (as
// is done here) but for the other cases, it should work in 4-byte
// chunks.

// REFERENCES:
//   [1] "DT4 Data File Format Specification" [July, 2010] DT4_format_2010.pdf

//#define DEBUG 1
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
    
// 1. Routines to maintain static storage to avoid possibly thousands
// of allocations of identical size, for a fixed purpose.
// FIXME: see if OK on multicore; if not, use R method (if that's ok).
static unsigned char *buffer = NULL; // single-beam uses just first 2 bytes of each 4-byte entry
void biosonics_allocate_storage(int spp, int byte_per_sample)
{
  if (buffer == NULL) {
#ifdef DEBUG
    Rprintf("should allocate space for %d data now\n", spp * byte_per_sample);
#endif
    buffer = (unsigned char*)calloc(spp * byte_per_sample, sizeof(unsigned char));
    if (buffer == NULL) {
      error("cannot allocate space for temporary buffer, of length %d bytes", spp * byte_per_sample);
    }
#ifdef DEBUG
    Rprintf("... allocation was OK; address is %lx\n", buffer);
#endif
  }
}

void biosonics_free_storage()
{
#ifdef DEBUG
    Rprintf("freeing up 'buffer' storage at address %lx\n", buffer);
#endif
  if (buffer != NULL)
    free(buffer);
  buffer = NULL;
}

// 2. handle RLE (run-length encoding) expansion, for 2-byte and
// 4-byte chunks
void rle2(unsigned short int *buf)
{
  error("rle2 not coded yet\n");
}

void rle4(unsigned char *samp, int ns, int spp)
{
  // This code is patterned on [1 p36-37] for runlength expansion
  // of data stored in 4-byte chunks.  The main difference is 
  // that the present function uses bytewise operations, which
  // means that it should work the same on big-endian
  // and little-endian computers.  After this function returns,
  // the global-storage 'buffer' contains runlength expanded
  // data taken from the 'samp', and the length of that buffer
  // is 4*spp bytes.
#ifdef DEBUG
  Rprintf("rle4(0x%02x%02x%02x%02x ..., ns=%d, spp=%d)\n", samp[0], samp[1], samp[2], samp[3], ns, spp);
#endif
  int i = 0, k = 0;
  unsigned char b1, b2, b3, b4;
  int ns4 = ns * 4;
  int spp4 = spp * 4;
  while (i < ns4) {
    //Rprintf("i=%d k=%d\n", i, k);
    b1 = samp[i++];
    b2 = samp[i++];
    b3 = samp[i++];
    b4 = samp[i++];
    if (b2 == 0xFF) {
      int n = (int)b1 + 2;
#ifdef DEBUG
      Rprintf("zero-fill %d samples at i=%d k=%d\n", n, i, k);
#endif
      while (n > 0) {
	if (k < spp4) {
	  buffer[k++] = 0x00;
	  buffer[k++] = 0x00;
	  buffer[k++] = 0x00;
	  buffer[k++] = 0x00;
          --n;
	} else { 
	  break; // prevent overfill (probably will never happen)
	}
      }
    } else {
      if (k < spp4) {
	buffer[k++] = b1;
	buffer[k++] = b2;
	buffer[k++] = b3;
	buffer[k++] = b4;
      } else {
	break;
      }
    }
  }
#ifdef DEBUG
  Rprintf("zero-fill at end for %d elements\n", (spp4-k)/4);
#endif
  while (k < spp4) {
    buffer[k++] = 0x00;
    buffer[k++] = 0x00;
    buffer[k++] = 0x00;
    buffer[k++] = 0x00;
  }
}


// 3. subsecond time for Biosonic echosounder
// [1 p19]
void biosonics_ss(unsigned char *byte, double *out)
{
  if (!(0x80 & *byte))
    *out = 0.0;
  else
    *out = (float)((int)(0x7F & *byte)) / 100;
}

// 4. decode biosonics two-byte floating format
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

SEXP biosonics_ping(SEXP bytes, SEXP Rspp, SEXP Rns, SEXP Rtype)
{
  PROTECT(bytes = AS_RAW(bytes));
  PROTECT(Rspp = AS_NUMERIC(Rspp));
  int spp = (int)floor(0.5 + *REAL(Rspp));
  PROTECT(Rns = AS_NUMERIC(Rns));
  int ns = (int)floor(0.5 + *REAL(Rns));
  PROTECT(Rtype = AS_NUMERIC(Rtype));
  int type = (int)floor(0.5 + *REAL(Rtype));
  //double *typep = REAL(type);
  //int beam = (int)floor(0.5 + *typep);
#ifdef DEBUG
  Rprintf("biosonics_ping() decoded type:%d, spp:%d, ns:%d\n", type, spp, ns);
#endif
  int byte_per_sample = 2;
  if (type == 1 || type == 2) {
    byte_per_sample = 4;
  }
  unsigned int nbytes = LENGTH(bytes);
#ifdef DEBUG
  Rprintf("nbytes: %d (should be 2*ns for single-beam or 4*ns for split- and dual-beam)\n", nbytes);
#endif
  unsigned char *bytep = RAW(bytes);
  unsigned int *uip = (unsigned int*)RAW(bytes);

  //Rprintf("unsigned int length %d; unsigned long int length %d; ptr %ld %ld\n", sizeof(unsigned int), sizeof(unsigned long int), bytep, uip);
  //Rprintf("nbytes %d\n", nbytes);
  SEXP res;
  SEXP res_names;
  PROTECT(res = allocVector(VECSXP, 1));
  PROTECT(res_names = allocVector(STRSXP, 1));
  SEXP res_a;
  PROTECT(res_a = allocVector(REALSXP, spp));
  biosonics_allocate_storage(spp, byte_per_sample);
  // Get R type storage (eventually will remove this)
#ifdef DEBUG
  Rprintf("allocVector(REALSXP, %d)\n", spp);
#endif
  double *resap = REAL(res_a);
  if (type == 1 || type == 2) {
    rle4(bytes, ns, spp);
    for (int k = 0; k < spp; k++) {
      // Quote [1 p37 re dual-beam]: "For an RLE-expanded sample x, the low-order
      // word (ie, (USHORT)(x & 0x0000FFFF)) contains the narrow-beam data. The
      // high-order word (ie, (USHORT)((x & 0xFFFF0000) >> 16)) contains the
      // wide beam data."
      //
      // Quote [1 p38 split-beam e.g. 01-Fish.dt4 example]: "the low-order word
      // (ie, (USHORT)(x & 0x0000FFFF)) contains the amplitude data. The
      // high-order byte (ie, (TINY)((x & 0xFF000000) >> 24)) contains the
      // raw X-axis angle data. The other byte
      // (ie, (TINY)((x & 0x00FF0000) >> 16)) contains the raw Y-axis angle data.
      resap[k] = biosonic_float(buffer[byte_per_sample * k], buffer[1 + byte_per_sample * k]);
    }
  } else {
    error("cannot handle single-beam data");
    for (int d = 0; d < nbytes / byte_per_sample; d++) { // FIXME: use 'ns' here to match docs
#ifdef DEBUG
      Rprintf(" %08X ", (unsigned int)uip[d]);
#endif
      //if ((uip[d] & 0xFF000000) == 0xFF000000) {
      //  Rprintf(" A. 4byte match to FF000000 at 4byte index %d or 1byte index %d\n", d, 4*d);
      //}
      //if ((uip[d] & 0x00FF0000) == 0x00FF0000) {
      //  Rprintf(" B. 4byte match to 00FF0000 at 4byte index %d or 1byte index %d\n", d, 4*d);
      //}
      if ((uip[d] & 0x0000FF00) == 0x0000FF00) {
	int n = (uip[d] & 0x000000FF) + 2;
#ifdef DEBUG
	Rprintf(" 4byte match of %08X to %08X at 4byte index %d or 1byte index %d; n=%d\n",
	    (unsigned int)uip[d], 0x0000FF00, d, 4*d, n);
#endif
      }
      //if ((uip[d] & 0x000000FF) == 0x000000FF) {
      //  Rprintf(" D. 4byte match to 000000FF at 4byte index %d or 1byte index %d\n", d, 4*d);
      //}
    }
    for (int ires = 0; ires < spp; ires++) {
      // zero fill at end, if needed
      if (nbytes <= (2*ires)) {
#ifdef DEBUG
	Rprintf("    padding %d data\n", (2*spp - nbytes)/2);
#endif
	while (ires < spp) {
	  resap[ires] = (double)0.0;
	  ires++;
	}
	break;
      }
      // The RLE starting code is 0xFF in an odd-numbered byte.
      // FIXME: does this work for 4byte data?
      if (bytep[1 + byte_per_sample * ires] == 0xFF) {
	int zeros = 2 + (int)bytep[byte_per_sample * ires];
#ifdef DEBUG
	Rprintf("RLE ires: %d, zeros: %d, spp: %d, flag-at: %d\n", ires, zeros, spp, 1+byte_per_sample*ires);
#endif
	if (ires + zeros >= spp)
	  zeros = spp - ires;
	for (int z = 0; z < zeros; z++) {
	  resap[ires] = 0.0;
	  ires++;
	}
      } else {
	// normal 2-byte datum
	resap[ires] = biosonic_float(bytep[byte_per_sample*ires], bytep[1+byte_per_sample*ires]);
      }
    }
  }
  SET_VECTOR_ELT(res, 0, res_a);
  SET_STRING_ELT(res_names, 0, mkChar("a"));
  setAttrib(res, R_NamesSymbol, res_names);
  UNPROTECT(7);
  return(res);
}


