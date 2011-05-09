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
  /* ldc_rdi = locate data chunk for RDI
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
  Rprintf("lbuf=%d, max=%d\n",lbuf,max_lres);
#endif
  /* Count matches, so we can allocate the right length */
  unsigned char byte1 = 0x7f;
  unsigned char byte2 = 0x7f; /* this equal 22 base 10, i.e. the number of bytes in record */
  unsigned int matches = 0;
  unsigned short int check_sum, desired_check_sum;
  unsigned int bytes_to_check = 0;
#ifdef DEBUG
  Rprintf("max_lres %d\n", max_lres);
#endif
  for (int i = 0; i < lbuf - 1; i++) { /* note that we don't look to the very end */
    if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
      if (matches == 0)
	bytes_to_check = pbuf[i+2] + 256 * pbuf[i+3];
      check_sum = 0;
      for (int c = 0; c < bytes_to_check; c++)
	check_sum += (unsigned short int)pbuf[i + c];
      desired_check_sum = ((unsigned short)pbuf[i+bytes_to_check+0]) | ((unsigned short)pbuf[i+bytes_to_check+1] << 8);
      if (check_sum == desired_check_sum) {
	matches++;
#ifdef DEBUG
	Rprintf("buf[%d] ok\n", i);
#endif
	if (max_lres != 0 && matches >= max_lres)
	  break;
      } else {
#ifdef DEBUG
	Rprintf("buf[%d] checksum %d (needed %d)\n", i, check_sum, desired_check_sum);
#endif
      }
    }
  }
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
      check_sum = 0;
      if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
	for (int c = 0; c < bytes_to_check; c++)
	  check_sum += (unsigned short int)pbuf[i + c];
	desired_check_sum = ((unsigned short)pbuf[i+bytes_to_check]) | ((unsigned short)pbuf[i+bytes_to_check+1] << 8);
	if (check_sum == desired_check_sum)
	  pres[ires++] = i + 1; /* the +1 is to get R pointers */
	if (ires >= lres)
	  break;
      }
    }
  } else {
    PROTECT(res = NEW_INTEGER(1));
    int *pres = INTEGER_POINTER(res);
    pres[0] = 0;
  }
  UNPROTECT(3);
  return(res);
}
