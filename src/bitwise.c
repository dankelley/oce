/* vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70: */

/*
 * References:
 * SIG2 = system-integrator-manual_Dec2014_jan.pdf
 */


#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

/* 
 * 1. compile from commandline:

 R CMD SHLIB bitwise.c

 * 2. test R code:

 buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
 dyn.load("bitwise.so"); m <- .Call("matchcheck2bytes", buf, c(as.raw(0xa5), as.raw(0x11)), 22, c(as.raw(0xb5), as.raw(0x8c)))
 print(m)

*/

SEXP unwrap_sequence_numbers(SEXP seq, SEXP bytes)
{
  /* "unwrap" a vector of integers that are sequence numbers wrapping in 'bytes' bytes, 
   * creating the sequence numbers that might have resulted, had 'seq' not been
   * created modulo 'bytes' bytes.
   *
   */
  PROTECT(seq = AS_INTEGER(seq));
  int *pseq = INTEGER_POINTER(seq);
  PROTECT(bytes = AS_INTEGER(bytes));
  int pbytes = *INTEGER_POINTER(bytes);
  long int pmod;
  if (pbytes == 2) {
    pmod = 65535 + 1;
  } else {
    error("only understand bytes=2 for now");
  }
  long int n = LENGTH(seq);
  SEXP res;
  PROTECT(res = NEW_INTEGER(n));
  int *pres = INTEGER_POINTER(res);
  long int last;
  long int cumulative = 0;
#ifdef DEBUG
  Rprintf("n=%ld\n", n);
#endif
  pres[0] = pseq[0];
  last = pseq[0];
  for (int i = 1; i < n; i++) {
    if (pseq[i] < last) {
      cumulative += pmod;
#ifdef DEBUG
      Rprintf("pseq[%d]=%d and last=%d, so updated to cumulative=%ld\n", i, pseq[i], last, cumulative);
#endif
    }
    pres[i] = pseq[i] + cumulative;
#ifdef DEBUG
    Rprintf("i=%d seq=%d rval=%d\n", i, pseq[i], pres[i]);
#endif
    last = pseq[i];
  }
  UNPROTECT(3);
  return(res);
}

SEXP ldc_sontek_adv_22(SEXP buf, SEXP max)
{
  /* ldc = locate data chunk; _sontek_adv = for a SonTek ADV (with temperature and/or pressure installed; see p95 of sontek-adv-op-man-2001.pdf)
   * BYTE   Contents
   *   1    0x85 [call this key1 in code]
   *   2    0x16 (length of record, 0x16 is 22 base 10) [ call this key2 in code]
   *   3:4  SampleNum, a little-endian unsigned integer. This should increase by 1 from sample
   *        to sample, and it wraps at value 65535
   *   5:6  x velocity component, signed 2-byte integer, in 0.1 mm/s [QUESTION: is there a scale factor issue?]
   *   7:8  y "
   *  9:10  z "
   *  11    beam 1 amplitude
   *  12    beam 2 "
   *  13    beam 3 "
   *  14    beam 1 correlation
   *  15    beam 2 "
   *  16    beam 3 "
   *  17:18 temperature (in 0.01 degC), signed little-endian integer
   *  19:20 pressure (in counts), signed little-endian integer
   *  21:22 checksum of bytes 1 to 20
   */

  /*
# testing

R CMD SHLIB bitwise.o ; R --no-save < a.R

# with a.R as follows:

f <- file('~/m05_sontek_adv','rb')
seek(f,0,"end")
n <- seek(f,0,"start")
buf <- readBin(f, "raw", n)
dyn.load("bitwise.so")
p <- .Call("ldc_sontek_adv_22", buf, 10)
np <- length(p)
pp <- sort(c(p, p+1)) # for two-byte reading
sample.number <- readBin(buf[pp+2], "integer", signed=FALSE, size=2, endian="little",n=np)
u1 <- 1e-4 * readBin(buf[pp+4], "integer", signed=TRUE, size=2, endian="little",n=np)
u2 <- 1e-4 * readBin(buf[pp+6], "integer", signed=TRUE, size=2, endian="little",n=np)
u3 <- 1e-4 * readBin(buf[pp+8], "integer", signed=TRUE, size=2, endian="little",n=np)
a1 <- readBin(buf[p+10], "integer", signed=TRUE, size=1, n=np)
a2 <- readBin(buf[p+11], "integer", signed=TRUE, size=1, n=np)
a3 <- readBin(buf[p+12], "integer", signed=TRUE, size=1, n=np)
c1 <- readBin(buf[p+13], "integer", signed=TRUE, size=1, n=np)
c2 <- readBin(buf[p+14], "integer", signed=TRUE, size=1, n=np)
c3 <- readBin(buf[p+15], "integer", signed=TRUE, size=1, n=np)
temperature <- 0.01 * readBin(buf[sort(c(p, p+1))+16], "integer", signed=TRUE, size=2, endian="little",n=np)
pressure <- readBin(buf[sort(c(p, p+1))+18], "integer", signed=TRUE, size=2, endian="little",n=np)
*/
  unsigned char *pbuf;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(max = AS_INTEGER(max));
  /* FIXME: check lengths of match and key */
  pbuf = RAW_POINTER(buf);
  int max_lres = *INTEGER_POINTER(max);
  int lres;
  int lbuf = LENGTH(buf);
  SEXP res;
#ifdef DEBUG
  Rprintf("lbuf=%d, max=%d\n",lbuf,max_lres);
#endif
  /* Count matches, so we can allocate the right length */
  unsigned char byte1 = 0x85;
  unsigned char byte2 = 0x16; /* this equal 22 base 10, i.e. the number of bytes in record */
  unsigned int matches = 0;
  unsigned short int check_sum_start = ((unsigned short)0xa5<<8)  | ((unsigned short)0x96); /* manual p96 says 0xA596; assume little-endian */
  unsigned short int check_sum, desired_check_sum;
  if (max_lres < 0)
    max_lres = 0;
  for (int i = 0; i < lbuf - byte2; i++) { /* note that we don't look to the very end */
    check_sum = check_sum_start;
    if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
#ifdef DEBUG
      Rprintf("tentative match %d at i = %d ... ", matches, i);
#endif
      for (int c = 0; c < 20; c++)
	check_sum += (unsigned short int)pbuf[i + c];
      desired_check_sum = ((unsigned short)pbuf[i+20]) | ((unsigned short)pbuf[i+21] << 8);
      if (check_sum == desired_check_sum) {
	matches++;
#ifdef DEBUG
	Rprintf("good match (check_sum=%d)\n", check_sum);
#endif
	if (max_lres != 0 && matches >= max_lres)
	  break;
      } else {
#ifdef DEBUG
	Rprintf("bad checksum\n");
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
    for (int i = 0; i < lbuf - byte2; i++) { /* note that we don't look to the very end */
      check_sum = check_sum_start;
      if (pbuf[i] == byte1 && pbuf[i+1] == byte2) { /* match first 2 bytes, now check the checksum */
	for (int c = 0; c < 20; c++)
	  check_sum += (unsigned short int)pbuf[i + c];
	desired_check_sum = ((unsigned short)pbuf[i+20]) | ((unsigned short)pbuf[i+21] << 8);
	if (check_sum == desired_check_sum) {
	  pres[ires++] = i + 1; /* the +1 is to get R pointers */
	}
	if (ires > lres)        /* FIXME: or +1? */
	  break;
      }
    }
    UNPROTECT(3);
    return(res);
  } else {
    PROTECT(res = NEW_INTEGER(1));
    int *pres = INTEGER_POINTER(res);
    pres[0] = 0;
    UNPROTECT(3);
    return(res);
  }
}



/*#define DEBUG*/
SEXP nortek_checksum(SEXP buf, SEXP key)
{
  /* http://www.nortek-as.com/en/knowledge-center/forum/current-profilers-and-current-meters/367698326 */
  /* 
     R CMD SHLIB bitwise.c 
     library(oce)
     f <- "/Users/kelley/data/archive/sleiwex/2008/moorings/m06/vector1943/194301.vec" ## dir will change; times are odd
     buf <- readBin(f, what="raw", n=1e4)
     vvd.start <- matchBytes(buf, 0xa5, 0x10)
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

SEXP match2bytes(SEXP buf, SEXP m1, SEXP m2, SEXP demand_sequential)
{
  int i, j, n, n_match, ds;
  double *resp;
  unsigned char *bufp, *m1p, *m2p;
  SEXP res;
  PROTECT(buf = AS_RAW(buf));
  PROTECT(m1 = AS_RAW(m1));
  PROTECT(m2 = AS_RAW(m2));
  PROTECT(demand_sequential = AS_INTEGER(demand_sequential));
  bufp = RAW_POINTER(buf);
  m1p = RAW_POINTER(m1);
  m2p = RAW_POINTER(m2);
  ds = *INTEGER(demand_sequential);
  n = LENGTH(buf);
  unsigned short seq_last=0, seq_this;
  // Rprintf("demand_sequential=%d\n",ds);
  int nnn=10;

  /* FIXME: the two passes repeat too much code, and should be done as a subroutine */

  //
  // Pass 1: allocate vector
  //
  n_match = 0;                  /* don't demand anything at start */
  for (i = 0; i < n - 1; i++) {
    if (bufp[i] == *m1p && bufp[i + 1] == *m2p) {
      if (ds) {
	seq_this = (((unsigned short)bufp[i + 3]) << 8) | (unsigned short)bufp[i + 2];
	// if (nnn > 0) Rprintf("i=%d seq_this=%d seq_last=%d ... ",i,seq_this,seq_last);
	if (!n_match || (seq_this == (seq_last + 1)) || (seq_this == 1 && seq_last == 65535)) { /* is second needed, given short type? */
	  n_match++;
	  ++i;			// skip
	  seq_last = seq_this;
	  // if (nnn > 0) Rprintf("KEEP\n");
	} else {
	  // if (nnn > 0) Rprintf("DISCARD\n");
	}
	//nnn--;
      } else {
	n_match++;
	++i;			// skip
      }
    }
  }
  //
  // Pass 2: fill in the vector 
  //
  PROTECT(res = NEW_NUMERIC(n_match));
  resp = NUMERIC_POINTER(res);
  j = 0;
  seq_last = 0;
  nnn = 1000;
  //Rprintf("PASS 2\n");
  n_match = 0;                  /* don't demand anything at start */
  for (i = 0; i < n - 1; i++) {
    //    Rprintf("[%d]:", i);
    if (bufp[i] == *m1p && bufp[i + 1] == *m2p) {
      if (ds) {
	seq_this = (((unsigned short)bufp[i + 3]) << 8) | (unsigned short)bufp[i + 2];
	//if (nnn > 0) Rprintf("i=%d seq_this=%d seq_last=%d ... ",i,seq_this,seq_last);
	if (!n_match || (seq_this == (seq_last + 1)) || (seq_this == 1 && seq_last == 65535)) { /* is second needed, given short type? */
	  n_match++;
	  resp[j++] = i + 1;	/* the 1 is to offset from C to R */
	  ++i;			/* skip */
	  seq_last = seq_this;
	  //if (nnn > 0) Rprintf("KEEP\n");
	} else {
	  //if (nnn > 0) Rprintf("DISCARD\n");
	}
	nnn--;
      } else {
	resp[j++] = i + 1;	/* the 1 is to offset from C to R */
	++i;			/* skip */
      }
    }
  }
  UNPROTECT(5);
  return(res);
}

/* NEW */
/*#define DEBUG 1*/
SEXP locate_vector_imu_sequences(SEXP buf)
{
  /*
   * imu = Inertial Motion Unit (system-integrator-manual_Dec2014_jan.pdf p30-32)
   *
   * *(buf)     0xa5
   * *(buf+1)   0x71
   * *(buf+2,3) int, # bytes in structure
   * There are 3 possibilities, keyed by *(buf+6)
   *
   * Case *(buf+6) Contents
   * ====|========|=======================================================================
   *    A 0xcc     Acceleration, Angular Rate, Magnetometer Vectors and Orientation Matrix
   *    B 0xd2     Gyro-stabilized Acceleration, Angular Rate and Magnetometer Vectors
   *    C 0xd3     DeltaAngle, DeltaVelocity and Magnetometer Vectors
   *
   * QUESTION: what is AHRSchecksum? do we check that? And what is
   * this second 'Checksum'?
   * Case A has checksum starting at offset 84 (sum of all words in structure)
   */

  /* 

     library(oce)
     system("R CMD SHLIB bitwise.c")
     dyn.load("bitwise.so")
     f <- "/Users/kelley/src/dolfyn/example_data/vector_data_imu01.VEC"
     buf <- readBin(f, what="raw", n=1e5) 
     a <- .Call("locate_vector_imu_sequences", buf)
     for (aa in a[1:10]) {
         message(paste(paste("0x", buf[aa+seq.int(0, 6L)], sep=""), collapse=" "))
     }
     ensembleCounter <- as.numeric(buf[a + 4])
     plot(seq_along(a), ensembleCounter, type='l')


     */
  PROTECT(buf = AS_RAW(buf));
  unsigned char *bufp;
  bufp = RAW_POINTER(buf);
  int bufn = LENGTH(buf);
  SEXP res;
  PROTECT(res = NEW_INTEGER(bufn)); // definitely more than enough space
  int *resp = INTEGER_POINTER(res);
  int resn = 0;
  int check=10; // check this many instance of 0xa5,0x71
  // We check 5 bytes, on the assumption that false positives will be
  // effectively zero then (1e-12, if independent random numbers
  // in range 0 to 255).
  // FIXME: test the checksum, but SIG2 does not state how.
  for (int i = 0; i < bufn-1; i++) {
    if (bufp[i] == 0xa5 && bufp[i+1] == 0x71) {
      if (check-- > 0) Rprintf("IMU testing ... buf[%d]=0x%02x; buf[%d+5]=0x%02x\n", i, bufp[i], i, bufp[i+5]);
      // Check at offset=5, which must be 1 of 3 choices.
      if (bufp[i+5] == 0xcc) {
	// length indication should be 0x2b=43=86/2 (SIG2, top of page 31)
	if (bufp[i+2] == 0x2b && bufp[i+3] == 0x00) {
	  resp[resn++] = i + 1; // add 1 for R notation
	  i++; //FIXME: skip to end, when we really trust identification
	}
      } else if (bufp[i+5] == 0xd2) { 
	// length indication should be 0x19=25=50/2 (SIG2, middle of page 31)
	if (bufp[i+2] == 0x19 && bufp[i+3] == 0x00) {
	  resp[resn++] = i + 1; // add 1 for R notation
	  i++; //FIXME: skip to end, when we really trust identification
	}
      } else if (bufp[i+5] ==0xd3) {
	// length indication should be 0x19=25=50/2 (SIG2, page 32)
	if (bufp[i+2] == 0x19 && bufp[i+3] == 0x00) {
	  resp[resn++] = i + 1; // add 1 for R notation
	  i++; //FIXME: skip to end, when we really trust identification
	}
      }
    }
  }
  SET_LENGTH(res, resn);
  UNPROTECT(2);
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
     f <- "/Users/kelley/data/archive/sleiwex/2008/moorings/m06/adv/nortek_1943/raw/adv_nortek_1943.vec"
     buf <- readBin(f, what="raw", n=1e6)
     vvd.start <- matchBytes(buf, 0xa5, 0x10)
     dyn.load("~/src/R-kelley/oce/src/bitwise.so")
     s <- .Call("locate_byte_sequences",buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c), 0)
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
#ifdef DEBUG
  Rprintf("lsequence=%d, lres=%d\n",lsequence,lres);
#endif
  /* Rprintf("max_lres=%d\n", max_lres); */
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
      if (ires >= lres)
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

// create (*n) unsigned 16-bit little-endian int values from 2*(*n) bytes, e.g.
// .C("uint16_le", as.raw(c(0x01, 0x02)), 1L, res=integer(1))$res
void uint16_le(unsigned char *b, int *n, int *out)
{
  for (int i = 0; i < *n; i++) {
    out[i] = (unsigned int)b[2*i] + 256 * (unsigned int)b[1+2*i];
  }
}

