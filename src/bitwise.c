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
  check_value = (((short)keyp[0]) << 8) | (short)keyp[1]; /* FIXME: should be -19060 Q: how to make a signed short from two hexes? */
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

SEXP matchcheck2bytes(SEXP buf, SEXP match, SEXP len, SEXP key)
{
  unsigned char *pbuf, *pmatch, *pkey;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(match = AS_RAW(match));
  PROTECT(len = AS_INTEGER(len));
  PROTECT(key = AS_RAW(key));
  /* FIXME: check lengths of match and key */
  pbuf = RAW_POINTER(buf);
  pmatch = RAW_POINTER(match);
  pkey = RAW_POINTER(key);
  int *plen = INTEGER_POINTER(len);
  Rprintf("len=%d\n",*plen);
  int lmatch = LENGTH(match);
  int lbuf = LENGTH(buf);
  int lkey = LENGTH(key);
  int lres = 0;
  /* Count matches, so we can allocate the right length */
  for (int i = 0; i < lbuf - 1; i++) {
    int found = 0;
    for (int m = 0; m < lmatch; m++) {
      if (pbuf[i+m] == pmatch[m]) 
        found++;
      else
        break;
    }
    if (found == lmatch) {
      lres++;
      short *check = (short*)(pbuf+i);
      short check_value = (((short)pkey[0]) << 8) | (short)pkey[1];
      if (lkey != 2) 
        error("key must be of length 2");
      Rprintf(" %d", check_value);
      for (int cc = 0; cc < (*plen) / 2 - 1; cc++) {
        check_value += *check++;
        Rprintf(" %d", check_value);
      }
      Rprintf("last 2 bytes: %02x %02x\n", pbuf[i+*plen-1], pbuf[i+*plen-2]);
      short check_sum = (((short)pbuf[i+*plen-1]) << 8) | (short)pbuf[i+*plen-2];
      Rprintf("\n");for (int d=0;d<*plen;d++)Rprintf("%02x ", pbuf[i+d]);Rprintf("\n");
      Rprintf("\ncheck_value=%d vs check_sum %d\n", check_value, check_sum);
      if (check_value == check_sum) Rprintf("GOOD %d\n", i + 1);
      i += lmatch - 1;           /* skip over matched bytes */
    }
  }
  /* BUG: should be checking the checksum above and below */
  /* load up the return vector */
  SEXP res;
  PROTECT(res = NEW_INTEGER(lres));
  int *pres = INTEGER_POINTER(res);
  int j = 0;
  for (int i = 0; i < lbuf - 1; i++) {
    if (j <= lres && pbuf[i] == pmatch[0] && pbuf[i + 1] == pmatch[1]) {
      pres[j++] = i + 1;	/* add 1 for origin of R vectors */
    }
  }
  UNPROTECT(5);
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
