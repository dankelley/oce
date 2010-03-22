#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* 
 * 1. compile from commandline:

 R CMD SHLIB bitwise.c
   
 * 2. test R code:
   
 buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
 dyn.load("bitwise.so"); m <- .Call("matchcheck2bytes", buf, c(as.raw(0xa5), as.raw(0x11)), 22, c(as.raw(0xb5), as.raw(0x8c)))
 print(m)

*/

/*#define DEBUG*/
SEXP nortek_checksum(SEXP buf, SEXP key)
{
  /* http://www.nortek-as.com/en/knowledge-center/forum/current-profilers-and-current-meters/367698326 */
  /* 
     R CMD SHLIB bitwise.c 
     library(oce)
f <- "/Users/kelley/data/archive/sleiwex/2008/moorings/m06/vector1943/194301.vec" ## dir will change; times are odd
buf <- readBin(f, what="raw", n=1e4)
vvd.start <- match.bytes(buf, 0xa5, 0x10)
ok <- NULL;dyn.load("~/src/R-kelley/oce/src/bitwise.so");for(i in 1:200) {ok <- c(ok, .Call("nortek_checksum",buf[vvd.start[i]+0:23], c(0xb5, 0x8c)))}
  */
  int i, n;
  short check_value;
  int *resp;
  unsigned char *bufp, *keyp;
  SEXP res;
  PROTECT(key = AS_RAW(key));
  PROTECT(buf = AS_RAW(buf));
  bufp = (unsigned char*)RAW_POINTER(buf);
  keyp = (unsigned char*)RAW_POINTER(key);
#ifdef DEBUG
  Rprintf("buf[0]=0x%02x\n",bufp[0]);
  Rprintf("buf[1]=0x%02x\n",bufp[1]);
  Rprintf("buf[2]=0x%02x\n",bufp[2]);
  Rprintf("key[0]=0x%02x\n", keyp[0]);
  Rprintf("key[1]=0x%02x\n", keyp[1]);
#endif
  n = LENGTH(buf);
  check_value = (((short)keyp[0]) << 8) | (short)keyp[1]; 
#ifdef DEBUG
  Rprintf("check_value= %d\n", check_value);
  Rprintf("n=%d\n", n);
#endif
  short *sbufp = (short*) bufp;
  for (i = 0; i < (n - 2)/2; i++) {
#ifdef DEBUG
    Rprintf("i=%d buf=0x%02x\n", i, sbufp[i]);
#endif
    check_value += sbufp[i];
#ifdef DEBUG
    Rprintf("after, check_value=%d\n", check_value);
#endif
  }
  short checksum;
  checksum = (((short)bufp[n-1]) << 8) | (short)bufp[n-2];
#ifdef DEBUG
  Rprintf("CHECK AGAINST 0x%02x 0x%02x\n", bufp[n-2], bufp[n-1]);
  Rprintf("CHECK AGAINST %d\n", checksum);
#endif
  PROTECT(res = NEW_LOGICAL(1));
  resp = LOGICAL_POINTER(res);
  *resp = check_value == checksum;
  UNPROTECT(3);
  return(res);
}

SEXP match2bytes(SEXP buf, SEXP m1, SEXP m2)
{
  int i, j, n, n_match;
  double *resp;
  unsigned char *bufp, *m1p, *m2p;
  SEXP res;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(m1 = AS_RAW(m1));
  PROTECT(m2 = AS_RAW(m2));
  bufp = RAW_POINTER(buf);
  m1p = RAW_POINTER(m1);
  m2p = RAW_POINTER(m2);
  n = LENGTH(buf);
  n_match = 0;
  for (i = 0; i < n - 1; i++) {
    if (bufp[i] == *m1p && bufp[i + 1] == *m2p) {
      n_match++;
      ++i;			/* skip */
    }
  }
  PROTECT(res = NEW_NUMERIC(n_match));
  resp = NUMERIC_POINTER(res);
  j = 0;
  for (i = 0; i < n - 1; i++) {
    if (j <= n_match && bufp[i] == *m1p && bufp[i + 1] == *m2p) {
      resp[j++] = i + 1;	/* the 1 is to offset from C to R */
    }
  }
  UNPROTECT(4);
  return(res);
}

/*#define DEBUG 1*/
SEXP locate_byte_sequences(SEXP buf, SEXP match, SEXP len, SEXP key, SEXP max)
{
  /*
   * locate_byte_sequences() = function to be used for e.g. nortek adp / adv files
   * buf = buffer to be scanned
   * match = set of bytes that mark start of sequences
   * len = length of sequence
   * key = key added to checksum, and to be checked against last 2 bytes of sequence
   * max = 0 to use whole buffer, positive integer to limit to that many matches
   */

  /* 
R CMD SHLIB bitwise.c 
  */
  /*
library(oce)
f <- "/Users/kelley/data/archive/sleiwex/2008/moorings/m06/vector1943/194301.vec" ## dir will change; times are odd
buf <- readBin(f, what="raw", n=1e6)
vvd.start <- match.bytes(buf, 0xa5, 0x10)
dyn.load("~/src/R-kelley/oce/src/bitwise.so")
s <- .Call("locate_byte_sequences",buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c))
print(s)
print(vvd.start)
  */
  unsigned char *pbuf, *pmatch, *pkey;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(match = AS_RAW(match));
  PROTECT(len = AS_INTEGER(len));
  PROTECT(key = AS_RAW(key));
  PROTECT(max = AS_INTEGER(max));
  /* FIXME: check lengths of match and key */
  pbuf = RAW_POINTER(buf);
  pmatch = RAW_POINTER(match);
  pkey = RAW_POINTER(key);
  int lsequence = *INTEGER_POINTER(len);
  int max_lres = *INTEGER_POINTER(max);
#ifdef DEBUG
  Rprintf("lsequence=%d\n",lsequence);
#endif
  int lmatch = LENGTH(match);
  int lbuf = LENGTH(buf);
  int lkey = LENGTH(key);
  if (lkey != 2) error("key length must be 2");
  int ires = 0, lres = (int)(lbuf / lsequence + 3); /* get some extra space; fill some with NA */
  SEXP res;
#if DEBUG
  Rprintf("lsequence=%d, lres=%d\n",lsequence,lres);
#endif
  Rprintf("max_lres=%d\n", max_lres);
  if (max_lres > 0)
    lres = max_lres;
  PROTECT(res = NEW_INTEGER(lres));
  int *pres = INTEGER_POINTER(res);
  /* Count matches, so we can allocate the right length */
  short lsequence2 = lsequence / 2;
  for (int i = 0; i < lbuf - lsequence; i++) {
    short check_value = (((short)pkey[0]) << 8) | (short)pkey[1];
    int found = 0;
    for (int m = 0; m < lmatch; m++) {
      if (pbuf[i+m] == pmatch[m]) 
        found++;
      else
        break;
    }
    if (found == lmatch) {
      /* FIXME: should bit-twiddle this to work on all endian types */
      short *check = (short*)(pbuf+i);
      /*Rprintf(" %d", check_value);*/
      for (int cc = 0; cc < lsequence2 - 1; cc++) { /* last 2-byte chunk is the test value */
        check_value += *check++;
        /*Rprintf(" %d", check_value);*/
      }
      short check_sum = (((short)pbuf[i+lsequence-1]) << 8) | (short)pbuf[i+lsequence-2];
#ifdef DEBUG
      Rprintf("i=%d lbuf=%d ires=%d  lres=%d  check_value=%d vs check_sum %d match=%d\n", i, lbuf, ires, lres, check_value, check_sum, check_value==check_sum);
#endif
      if (check_value == check_sum) {
        pres[ires++] = i + 1;
        i += lsequence - lmatch; /* no need to check within sequence */
      }
      if (ires >= lres - 1)
        break;
    }
    i += lmatch - 1;           /* skip over matched bytes */
    if (i > (lbuf - lsequence)) 
      break; /* FIXME: can this ever happen? */
  }
  SET_LENGTH(res, ires);
  UNPROTECT(6);
  return(res);
}

SEXP match3bytes(SEXP buf, SEXP m1, SEXP m2, SEXP m3)
{
  int i, j, n, n_match;
  double *resp;
  unsigned char *bufp, *m1p, *m2p, *m3p;
  SEXP res;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(m1 = AS_RAW(m1));
  PROTECT(m2 = AS_RAW(m2));
  PROTECT(m3 = AS_RAW(m3));
  bufp = RAW_POINTER(buf);
  m1p = RAW_POINTER(m1);
  m2p = RAW_POINTER(m2);
  m3p = RAW_POINTER(m3);
  n = LENGTH(buf);
  n_match = 0;
  for (i = 0; i < n - 2; i++) {
    if (bufp[i] == *m1p && bufp[i + 1] == *m2p && bufp[i + 2] == *m3p) {
      n_match++;
      ++i;			/* skip */
      ++i;			/* skip */
    }
  }
  PROTECT(res = NEW_NUMERIC(n_match));
  resp = NUMERIC_POINTER(res);
  j = 0;
  for (i = 0; i < n - 2; i++) {
    if (j <= n_match && bufp[i] == *m1p && bufp[i + 1] == *m2p && bufp[i + 2] == *m3p) {
      resp[j++] = i + 1;	/* the 1 is to offset from C to R */
    }
  }
  UNPROTECT(5);
  return(res);
}
