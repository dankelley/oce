/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

// #define DEBUG

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector match2bytes(RawVector buf, RawVector m1, RawVector m2,
                          IntegerVector demand_sequential) {
  int ds = demand_sequential[0];
  long long int i, j, n_match = 0, n = buf.size();
#ifdef DEBUG
  Rprintf("n=%lld ds=%d\n", n, ds);
#endif
  unsigned short seq_last = 0, seq_this;
  for (i = 0; i < n - 1; i++) {
#ifdef DEBUG
    Rprintf("  pass 1: buf[%lld]=0x%02x, m1=0x%02x, m2=0x%02x\n", i, buf[i],
            m1[0], m2[0]);
#endif
    if (buf[i] == m1[0] && buf[i + 1] == m2[0]) {
#ifdef DEBUG
      Rprintf("    match at i=%lld\n", i);
#endif
      if (ds) {
        seq_this =
            (((unsigned short)buf[i + 3]) << 8) | (unsigned short)buf[i + 2];
        if (!n_match || (seq_this == (seq_last + 1)) ||
            (seq_this == 1 &&
             seq_last == 65535)) { /* is second needed, given short type? */
          n_match++;
          ++i; // skip
          seq_last = seq_this;
        }
      } else {
        n_match++;
        ++i; // skip
      }
    }
  }
#ifdef DEBUG
  Rprintf("  at end of pass 1, have i=%lld and n_match=%lld\n", i, n_match);
#endif
  // Pass 2: fill in the vector
  NumericVector res(n_match);
  j = 0;
  seq_last = 0;
  n_match = 0; /* don't demand anything at start */
  for (i = 0; i < n - 1; i++) {
#ifdef DEBUG
    Rprintf("  pass 2 buf[%lld]=0x%02x, m1=0x%02x, m2=0x%02x\n", i, buf[i],
            m1[0], m2[0]);
#endif
    if (buf[i] == m1[0] && buf[i + 1] == m2[0]) {
      if (ds) {
        seq_this =
            (((unsigned short)buf[i + 3]) << 8) | (unsigned short)buf[i + 2];
        if (!n_match || (seq_this == (seq_last + 1)) ||
            (seq_this == 1 &&
             seq_last == 65535)) { /* is second needed, given short type? */
#ifdef DEBUG
          Rprintf("    match case 1 at i=%lld, j=%lld\n", i, j);
#endif
          n_match++;
          res[j++] = i + 1; /* the 1 is to offset from C to R */
          ++i;              /* skip */
          seq_last = seq_this;
        }
      } else {
#ifdef DEBUG
        Rprintf("    match case 2 at i=%lld, j=%lld\n", i, j);
#endif
        res[j++] = i + 1; // the 1 is to offset from C to R
        ++i;              /* skip */
      }
    }
  }
  return (res);
} // NumericVector

// [[Rcpp::export]]
NumericVector match3bytes(RawVector buf, RawVector m1, RawVector m2,
                          RawVector m3) {
  long long int i, j, n = buf.size(), n_match = 0;
  for (i = 0; i < n - 2; i++) {
    if (buf[i] == m1[0] && buf[i + 1] == m2[0] && buf[i + 2] == m3[0]) {
      n_match++;
      i = i + 2; // skip
    }
  }
  NumericVector res(n_match);
  j = 0;
  for (i = 0; i < n - 2; i++) {
    if (j <= n_match && buf[i] == m1[0] && buf[i + 1] == m2[0] &&
        buf[i + 2] == m3[0]) {
      res[j++] = i + 1; // add 1 to get an index-1 R value
    }
  }
  return (res);
} // NumericVector
