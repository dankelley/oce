// vim: set expandtab shiftwidth=2 softtabstop=2 tw=70:

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using std::vector;

#define DEBUG 1

// [[Rcpp::export]]
NumericVector unwrapSequenceNumbers(IntegerVector seq, IntegerVector bytes) {
  // "unwrap" a vector of integers that are sequence numbers wrapping in 'bytes'
  // bytes, creating the sequence numbers that might have resulted, had 'seq'
  // not been created modulo 'bytes' bytes.
  long long int mod, n = seq.size(), last, cumulative = 0;
  if (bytes[0] == 2) {
    mod = 65535 + 1;
  } else {
    ::Rf_error("only understand bytes=2 for now");
  }
#ifdef DEBUG
  Rprintf("NEW n=%lld\n", n);
#endif
  vector<double> tmpres;
  tmpres.reserve(n);
  tmpres.push_back(seq[0]);
  last = seq[0];
  for (long long int i = 1; i < n; i++) {
    if (seq[i] < last) {
      cumulative += mod;
#ifdef DEBUG
      Rprintf("seq[%lld]=%d and last=%lld, so updated to cumulative=%lld\n",
              i, seq[i], last, cumulative);
#endif
    }
    tmpres.push_back(seq[i] + cumulative);
#ifdef DEBUG
    Rprintf("at i=%lld, have seq=%d and tmpres=%.1f\n", i, seq[i], tmpres[i]);
#endif
    last = seq[i];
  }
  NumericVector res(tmpres.size());
  for (long long unsigned int i = 0; i < tmpres.size(); i++) {
    res[i] = tmpres[i];
  }
  return (res);
} // unwrapSequenceNumbers

// [[Rcpp::export]]
NumericVector ldcSontekAdv22(RawVector buf, IntegerVector max) {
  /* ldc = locate data chunk in data from a Sontek adv with temperature
   * and/or pressure installed; see p95 of sontek-adv-op-man-2001.pdf
   *
   * BYTE Contents
   *
   *     1 0x85 [call this key1 in code]
   *
   *     2 0x16 (length of record, 0x16 is 22 base 10) [call this key2 in code]
   *
   *   3:4 SampleNum, a little-endian unsigned integer. This increases
   *       by 1 from one sample to the next, except that it wraps at 65535.
   *
   *   5:6 x velocity component, signed 2-byte integer, in 0.1 mm/s [QUESTION:
   *       is there a scale factor issue?]
   *
   *   7:8 y "
   *
   *  9:10 z "
   *
   * 11:13 beam 1 to 3 amplitude
   *
   * 14:16 beam 1 to 3 correlation
   *
   * 17:18 temperature (in 0.01 degC), signed little-endian integer
   *
   * 19:20 pressure (in counts), signed little-endian integer
   *
   * 21:22 checksum of bytes 1 to 20
   */
  long long int i, lbuf = buf.size();
  long long unsigned int max_lres = (long long unsigned int)max[0];
  if (max_lres <= 0) {
    max_lres = lbuf;
  }
#ifdef DEBUG
  Rprintf("lbuf=%lld, max_lres=%lld\n", lbuf, max_lres);
#endif
  unsigned char byte1 = 0x85;
  unsigned char byte2 = 0x16; // = 22 base 10, number bytes per record
  unsigned short int check_value_start =
      ((unsigned short)0xa5 << 8) |
      ((unsigned short)0x96); /* manual p96 says 0xA596; assume little-endian */
  unsigned short int check_sum, check_value;
  if (max_lres < 0) {
    max_lres = 0;
  }
  // note that we don't look to the very end
  vector<double> tmpres;
  tmpres.reserve(max_lres);
  for (i = 0; i < lbuf - byte2; i++) {
    check_value = check_value_start;
    // match first 2 bytes, now check the checksum
    if (buf[i] == byte1 && buf[i + 1] == byte2) {
      for (int cc = 0; cc < byte2 - 2; cc += 2) {
        check_value += ((short)buf[i + cc]) | ((short)buf[i + cc + 1] << 8);
      }
      check_sum =
          ((unsigned short)buf[i + 20]) | ((unsigned short)buf[i + 21] << 8);
      if (check_value == check_sum) {
#ifdef DEBUG
        Rprintf("good match at i=%lld (check_sum=%d)\n", i, check_sum);
#endif
        tmpres.push_back(i + 1); // the +1 is to get R pointers
        if (max_lres != 0 && tmpres.size() >= max_lres) {
          break;
        }
      } else {
#ifdef DEBUG
        Rprintf("bad checksum at i=%lld, so skipping ahead\n", i);
#endif
      }
    }
  }
  NumericVector res(tmpres.size());
  for (long long unsigned i = 0; i < tmpres.size(); i++) {
    res[i] = tmpres[i];
  }
  return (res);
} // ldcSontekAdv22
