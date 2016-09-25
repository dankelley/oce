/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/* 

   This code was based on similar for sontek.  But, the best plan is
   always to figure out the algorithm (how many bytes to skip, etc.)
   using R.  Below are some notes on how I did that; I'll retain them in
   the code as a guide.  All of the below is in R (but some comments are
   instructions).

   library(oce)
# Temporarily instrument read.adp.rdi() to output ''b'' for ''buf'' in the code.
source("../R/adp.rdi.R")
d <- read.adp.rdi('/data/archive/sleiwex/2008/moorings/m11/adp/rdi_10485/raw/adp_rdi_10485.000',from=1,to=1000)
# Now 'b' holds the data.  Find some trial start-points by matching bytes:
p <- .Call("match2bytes", b, 0x7f, 0x7f, !TRUE)
bytes.in.ensemble <- as.numeric(b[p[1]+2]) + 256*as.numeric(b[p[1]+3])
# check some checksums
for (i in 1:10) {
calc <- sum(as.numeric(b[p[i]+seq(0, bytes.in.ensemble-1)]))%%2^16
check <- as.numeric(b[p[i]+bytes.in.ensemble]) + 256 * as.numeric(b[p[i]+bytes.in.ensemble+1])
cat(i, calc, check, if(calc==check) "OK\n" else "BAD\n")
}

*/

SEXP ldc_rdi(SEXP buf, SEXP max)
{
  /* Locate Data Chunk for RDI
   *   buf = buffer
   *   max = 0 in normal use, but can be >0 to test things manually
   *
   * Ref: WorkHorse Commands and Output Data Format_Nov07.pdf
   * p124: header structure (note that 'number of bytes in ensemble'
   *       does *not* count the first 2 bytes; it's really an offset to the
   *       checksum)
   * p158 (section 5.8) checksum
   */
  PROTECT(buf = AS_RAW(buf));
  PROTECT(max = AS_INTEGER(max));
  /* FIXME: check lengths of match and key */
  unsigned char *pbuf = RAW_POINTER(buf);
  int max_lres = *INTEGER_POINTER(max);
  if (max_lres < 0)
    error("'max' must be positive");
  int lres;
  int lbuf = LENGTH(buf);
  SEXP res;
#ifdef DEBUG
  Rprintf("lbuf=%d\n", lbuf);
#endif
  /* Count matches, so we can allocate the right length */
  unsigned char byte1 = 0x7f;
  unsigned char byte2 = 0x7f; /* this equal 22 base 10, i.e. the number of bytes in record */
  unsigned int matches = 0;
  unsigned short int check_sum, desired_check_sum;
  unsigned int bytes_to_check = 0;
#ifdef DEBUG
  Rprintf("max_lres %d (this is the 'max' given as an arg -- it is used only in some manual debugging and will be zero normally)\n", max_lres);
#endif
  for (int i = 0; i < lbuf - 1; i++) { /* note that we don't look to the very end */
    if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
      //if (matches == 0) {
	bytes_to_check = pbuf[i+2] + 256 * pbuf[i+3];
#ifdef DEBUG
	//if (matches < 30) Rprintf("matches=%d; buf[%d] bytes_to_check %d\n", matches, i, bytes_to_check);
#endif
	//}
      if ((i + bytes_to_check) < lbuf) {
	check_sum = 0;
	for (int c = 0; c < bytes_to_check; c++) {
	  check_sum += (unsigned short int)pbuf[i + c];
	}
	desired_check_sum = ((unsigned short int)pbuf[i+bytes_to_check+0]) | ((unsigned short int)pbuf[i+bytes_to_check+1] << 8);
	if (check_sum == desired_check_sum) {
	  matches++;
#ifdef DEBUG
	  //if (matches < 30) Rprintf("matches=%d; buf[%d] correct checksum %d (needed %d)\n", matches, i, check_sum, desired_check_sum);
	  if (matches < 30) Rprintf("i=%d; bytes_to_check=%d\n", i, bytes_to_check);
#endif
	  if (max_lres != 0 && matches >= max_lres) {
	    break;
	  }
	} else {
#ifdef DEBUG
	  //if (matches < 30) Rprintf("matches=%d; buf[%d] incorrect checksum %d (needed %d)\n", matches, i, check_sum, desired_check_sum);
#endif
	}
      }
    }
  }
  R_CheckUserInterrupt();
  /* allocate space, then run through whole buffer again, noting the matches */
  lres = matches;
  if (lres > 0) {
    PROTECT(res = NEW_INTEGER(lres));
    int *pres = INTEGER_POINTER(res);
#ifdef DEBUG
    Rprintf("getting space for %d matches\n", lres);
#endif
    unsigned int ires = 0;
    for (int i = 0; i < lbuf - 1; i++) { /* note that we don't look to the very end */
      bytes_to_check = pbuf[i+2] + 256 * pbuf[i+3];
      if ((i + bytes_to_check) < lbuf) {
#ifdef DEBUG
	if ((bytes_to_check + i) >= (lbuf - 10)) Rprintf("CAUTION will get close to buffer end; space= %d\n", lbuf - (bytes_to_check + i));
#endif
	check_sum = 0;
	if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
	  for (int c = 0; c < bytes_to_check; c++)
	    check_sum += (unsigned short int)pbuf[i + c];
	  desired_check_sum = ((unsigned short int)pbuf[i+bytes_to_check]) | ((unsigned short int)pbuf[i+bytes_to_check+1] << 8);
	  if (check_sum == desired_check_sum)
	    pres[ires++] = i + 1; /* the +1 is to get R pointers */
	}
	if (ires >= lres) {
#ifdef DEBUG
	  Rprintf("got to end of pres buffer, breaking now: DOES THIS EXIT LOOP?\n");
#endif
	  break;
	}
      }
    }
  } else {
    PROTECT(res = NEW_INTEGER(1));
    int *pres = INTEGER_POINTER(res);
#ifdef DEBUG
    Rprintf("lres <= 0; setting pres to 0 (does that get checked?)\n");
#endif
    pres[0] = 0;
  }
  UNPROTECT(3);
  return(res);
}
