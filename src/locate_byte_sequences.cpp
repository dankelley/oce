// vim: set expandtab shiftwidth=2 softtabstop=2 tw=70:

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using std::vector;

// #define DEBUG 1

// References:
// SIG2 = system-integrator-manual_Dec2014_jan.pdf

// Test/demo used in development
#if 0
library(oce)
f <- "~/git/oce/tests/testthat/local_data/adv_nortek_vector"
buf <- readBin(f, what = "raw", n = 1e6)
old <- .Call("locate_byte_sequences_old", buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c), 0)
#NB.this is not exported; it is designed for internal use.
new <- oce:::locateByteSequences(buf, c(0xa5, 0x10), 24, c(0xb5, 0x8c), 0)
#The as.integer is because the new code returns a double(to permit
#very large files).
testthat::expect_equal(old, as.integer(new))
#endif

// [[Rcpp::export]]
NumericVector locateByteSequences(RawVector buf, RawVector match,
                                  IntegerVector len, RawVector key,
                                  IntegerVector max) {
  /*
   * A function used for e.g. nortek adp/adv files, with the following
   * parameters.
   *
   * buf = raw buffer to be scanned
   *
   * match = set of bytes that mark start of sequences (must be of
   * length 2)
   *
   * len = length of sequence
   *
   * key = a quantity added to the checksum, the result of which is
   * checked against last 2 bytes of the sequence
   *
   * max = 0 to use whole buffer, positive integer to limit to that many matches
   */

  long long int lsequence = len[0]; // *INTEGER_POINTER(len);
  int lmatch = match.size();        // LENGTH(match);
  long long int lbuf = buf.size();  // LENGTH(buf);
  int lkey = key.size();            // LENGTH(key);
  if (lkey != 2) {
    ::Rf_error("key length must be 2, but it is %d", lkey);
  }
  // get some extra space; fill some with NA
  long long int ires = 0;
  long long int lres = (long int)(lbuf / len[0] + 3);
#ifdef DEBUG
  Rprintf("NEW: len[0]=%d, max=%d lres=%lld\n", len[0], max[0], lres);
#endif
  if (max[0] == 0) {
    max[0] = lbuf;
  }
#ifdef DEBUG
  Rprintf("NEW: later len[0]=%d, max=%d lres=%d\n", len[0], max[0], lres);
#endif
  // double *tmpres = (double *)R_Calloc((size_t)max[0], double);
  vector<long long int> tmpres;
  tmpres.reserve((size_t)max[0]);
#ifdef DEBUG
  Rprintf(" NEW: reserved vector space for %d elements\n", tmpres.size());
#endif
  // Count matches, so we can allocate the right length
#ifdef DEBUG
  Rprintf(" NEW: lsequence=%d\n", lsequence);
#endif
  for (long long int i = 0; i < lbuf - lsequence; i++) {
    short check_value = (((short)key[0]) << 8) | (short)key[1];
#ifdef DEBUG
    if (i == 0) {
      Rprintf(" NEW initially, check_value = %d\n", check_value);
    }
#endif
    int found = 0;
    for (int m = 0; m < lmatch; m++) {
      if (buf[i + m] == match[m]) {
        found++;
#ifdef DEBUG
        // if (i < 3000) {
        // Rprintf(" m=%d, found=%d\n", m, found);
        // }
#endif
      } else {
#ifdef DEBUG
        // if (i < 3000) {
        // Rprintf(" breaking\n");
        // }
#endif
        break;
      }
    }
    if (found == lmatch) {
#ifdef DEBUG
      if (i < 3000) {
        Rprintf(" found=%d == lmatch=%d\n", found, lmatch);
      }
#endif
      // FIXME: should bit-twiddle this to work on all endian types
      //?short *check = (short *)buf[i];
      // Rprintf(" %d", check_value);
      // last 2-byte chunk is the test value
      for (int cc = 0; cc < lsequence - 2; cc += 2) {
        check_value += ((short)buf[i + cc]) | ((short)buf[i + cc + 1] << 8);
#ifdef DEBUG
        if (ires == 0) {
          Rprintf("NEW: at cc=%d, check_value = %d\n", cc, check_value);
        }
#endif
#ifdef DEBUG
        // Rprintf(" cc=%d check_value=%d\n", cc, check_value);
#endif
      }
      short check_sum = (((short)buf[i + lsequence - 1]) << 8) |
                        (short)buf[i + lsequence - 2];
#ifdef DEBUG
      if (ires == 0) {
        Rprintf("NEW: check_sum = %d\n", check_sum);
      }
#endif
#ifdef DEBUG
      if (i < 3000) {
        Rprintf(
            "i=%d ires=%d  lres=%d  check_value=%d vs check_sum %d match=%d\n",
            i, ires, lres, check_value, check_sum, check_value == check_sum);
      }
#endif
      if (check_value == check_sum) {
#ifdef DEBUG
        if (ires == 0) {
          Rprintf("NEW: MATCH ires=%d check_value = check_sum = %d\n", ires,
                  check_sum);
        }
#endif
#ifdef DEBUG
        Rprintf("NEW: pushing to tmpres[%d] = %lld\n", ires, i + 1);
#endif
        tmpres.push_back(i + 1);
        ires++;
        i += lsequence - lmatch; // no need to check within sequence
      }
      if (ires >= lres) {
        break;
      }
    }
    i += lmatch - 1; // skip over matched bytes
    if (i > (lbuf - lsequence)) {
      break; // FIXME: can this ever happen?
    }
  }
#ifdef DEBUG
  Rprintf("NEW: ires=%d\n", ires);
#endif
  NumericVector res(tmpres.size());
  for (long long unsigned int i = 0; i < tmpres.size(); i++) {
    res[i] = tmpres[i];
  }
  return (res);
} // locateByteSequences

// [[Rcpp::export]]
NumericVector locateVectorImuSequences(RawVector buf) {
  /*
   * imu = Inertial Motion Unit (system-integrator-manual_Dec2014_jan.pdf
   * p30-32)
   *
   * *(buf)     0xa5
   * *(buf+1)   0x71
   * *(buf+2,3) int, # bytes in structure
   * There are 3 possibilities, keyed by *(buf+6), "K", say
   *
   * Case |  K   | Contents
   * =====|======|=====================================================================
   *   A  | 0xc2 | ?
   *   B  | 0xcc | Acceleration, Angular Rate, Magnetometer Vectors, Orientation
   * Matrix C  | 0xd2 | Gyro-stabilized Acceleration, Angular Rate, Magnetometer
   * Vectors D  | 0xd3 | DeltaAngle, DeltaVelocity, Magnetometer Vectors
   *
   * QUESTION: what is AHRSchecksum? do we check that? And what is
   * this second 'Checksum'?
   * Case A has checksum starting at offset 84 (sum of all words in structure)
   */

  long long int i, bufn = buf.size();
  vector<long long int> tmpres;
  // int check=10; // check this many instance of 0xa5,0x71
  //  We check 5 bytes, on the assumption that false positives will be
  //  effectively zero then (1e-12, if independent random numbers
  //  in range 0 to 255).
  //  FIXME: test the checksum, but SIG2 does not state how.

  for (i = 0; i < bufn - 1; i++) {
    if (buf[i] == 0xa5 && buf[i + 1] == 0x71) {
      // if (check-- > 0) Rprintf("IMU test: buf[%d]=0x%02x, buf[%d+2]=0x%02x,
      // buf[%d+5]=0x%02x\n", i, bufp[i], i, bufp[i+2], i, bufp[i+5]);
      //  Check at offset=5, which must be 1 of 3 choices.
      if (buf[i + 5] == 0xc3) {
        // FIXME: should verify this length check, which I got by inspecting
        // dolfyn code and a file provided privately in March 2016.
        if (buf[i + 2] == 0x24 && buf[i + 3] == 0x00) {
          tmpres.push_back(i + 1); // add 1 for R notation
          i++; // FIXME: skip to end, when we really trust identification
        }
      } else if (buf[i + 5] == 0xcc) {
        // length indication should be 0x2b=43=86/2 (SIG2, top of page 31)
        if (buf[i + 2] == 0x2b && buf[i + 3] == 0x00) {
          tmpres.push_back(i + 1); // add 1 for R notation
          i++; // FIXME: skip to end, when we really trust identification
        }
      } else if (buf[i + 5] == 0xd2) { // decimal 210
        // length indication should be 0x19=25=50/2 (SIG2, middle of page 31)
        if (buf[i + 2] == 0x19 && buf[i + 3] == 0x00) {
          tmpres.push_back(i + 1); // add 1 for R notation
          i++; // FIXME: skip to end, when we really trust identification
        }
      } else if (buf[i + 5] == 0xd3) { // decimal 211
        // length indication should be 0x19=25=50/2 (SIG2, page 32)
        if (buf[i + 2] == 0x19 && buf[i + 3] == 0x00) {
          tmpres.push_back(i + 1); // add 1 for R notation
          i++; // FIXME: skip to end, when we really trust identification
        }
      }
    }
  }
  NumericVector res(tmpres.size());
  for (long long unsigned int i = 0; i < tmpres.size(); i++) {
    res[i] = tmpres[i];
  }
  return (res);
} // locateVectorImuSequences
