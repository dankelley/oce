/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */
//#define SHOW(ires) (4 > abs((int)ires-29556))

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>


//#define DEBUG

/* 

   Below are some notes on how I tested out some of the ideas in R.
   They are probably not very useful in understanding the C code, and
   are very unlikely to be as robust as that C code ... so they will likely
   be removed at some time.

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

SEXP ldc_rdi_in_buffer(SEXP buf, SEXP max)
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
  /* Count matches, so we can allocate the right length */
  unsigned char byte1 = 0x7f;
  unsigned char byte2 = 0x7f; /* this equal 22 base 10, i.e. the number of bytes in record */
  int matches = 0;
  unsigned short int check_sum, desired_check_sum;
  unsigned int bytes_to_check = 0;
  // Step 1: count matches (i.e. determine lres)
  for (int i = 0; i < lbuf - 1; i++) { /* note that we don't look to the very end */
    if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
      R_CheckUserInterrupt();
      bytes_to_check = (unsigned int)(pbuf[i+2]) + 256 * (unsigned int)(pbuf[i+3]);
      if ((i + bytes_to_check) < lbuf) {
        check_sum = 0;
        for (int ic = 0; ic < bytes_to_check; ic++) {
          check_sum += (unsigned short int)pbuf[i + ic];
          //if (matches < 1 && (ic < 5 || ic > (bytes_to_check-10)))
          //if (matches==LOOK) Rprintf("  ic %d, byte 0x%02x, check_sum %d [OLD METHOD]\n", ic, pbuf[i+ic], check_sum);
        }
        desired_check_sum = ((unsigned short int)pbuf[i+bytes_to_check]) | ((unsigned short int)pbuf[i+bytes_to_check+1] << 8);
        if (check_sum == desired_check_sum) {
          matches++;
          if (max_lres != 0 && matches >= max_lres) {
            break;
          }
        } else {
          //Rprintf("matches=%d; buf[%d] incorrect checksum %d (needed %d)\n", matches, i, check_sum, desired_check_sum);
        }
      }
      i += bytes_to_check+1; // skip to the next ensemble
    }
  }
  // Step 2: allocate space, then run through whole buffer again, noting the matches
  lres = matches;
  //Rprintf("OLD: got %d matches\n", matches);
  if (lres > 0) {
    PROTECT(res = NEW_INTEGER(lres));
    int *pres = INTEGER_POINTER(res);
    for (int i = 0; i < lres; i++)
      pres[i] = 0; // set to zero as a check
    int ires = 0;
    for (int i = 0; i < lbuf - 1; i++) { /* note that we don't look to the very end */
      if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
        R_CheckUserInterrupt();
        bytes_to_check = (unsigned int)pbuf[i+2] + 256 * (unsigned int)pbuf[i+3];
        //if (SHOW(ires)) Rprintf("OLD ires=%d i=%d b1=%d b2=%d bytes_to_check=%d\n",
        // ires, i, (int)pbuf[i+2], (int)pbuf[i+3], bytes_to_check);
        //if (bytes_to_check > 1000) Rprintf("OLD i=%d ires=%d odd b1=%d b2=%d bytes_to_check=%d\n",
        //    i, ires, (int)pbuf[i+2], (int)pbuf[i+3], bytes_to_check);
        if ((i + bytes_to_check) < lbuf) {
          check_sum = 0;
          for (int ic = 0; ic < bytes_to_check; ic++) {
            check_sum += (unsigned short int)pbuf[i + ic];
            //if (SHOW(ires)) Rprintf("OLD ires=%d ic=%d check_sum=%d\n", ires, ic, check_sum);
          }
          desired_check_sum = ((unsigned short int)pbuf[i+bytes_to_check]) | ((unsigned short int)pbuf[i+bytes_to_check+1] << 8);
          //if (SHOW(ires)) Rprintf("OLD ires=%d check_sum=%d desired_check_sum=%d bytes_to_check=%d\n",
          //ires, check_sum, desired_check_sum, bytes_to_check);
          if (check_sum == desired_check_sum) {
            pres[ires++] = i + 1; /* the +1 is to get R pointers */
          } else {
            // if (ires==LOOK) Rprintf("no match at i=%d\n", i);
          }
        }
        if (ires >= lres) {
          //Rprintf("OLD: got to end of pres buffer (i=%d ires=%d lres=%d), breaking now\n", i, ires, lres);
          break;
        }
        i += bytes_to_check+1; // skip to the next ensemble
        if (pbuf[i+1] != byte1) Rprintf("pbuf[%d] is 0x%02x, not 0x%02x\n", i+1, pbuf[i+1], byte1);
        if (pbuf[i+2] != byte1) Rprintf("pbuf[%d] is 0x%02x, not 0x%02x\n", i+2, pbuf[i+2], byte1);
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

