/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <Rcpp.h>
using namespace Rcpp;

// Cross-reference work:
// 1. update registerDynamicSymbol.c if the number of argumentss is changed
// 2. main code should use the autogenerated wrapper in ../R/RcppExports.R
//
// [[Rcpp::export]]
NumericVector do_gappy_index(NumericVector starts, NumericVector offset,
                             NumericVector length) {
  long int Offset = static_cast<long int>(offset[0]);
  long int Length = static_cast<long int>(length[0]);
  long int nstarts = static_cast<long int>(starts.size());
  long int n = nstarts * Length;
  long int k = 0;
  NumericVector res(n);
  // Rprintf("In do_gappy_index(): nstarts=%ld, offset[0]=%d, length[0]=%d,
  // n=%ld\n", nstarts, Offset, Length, n);
  if (nstarts > 0) { // note that we start large
    long int minspan = 100 * nstarts * Length;
    // Rprintf("initial minspan = %ld (n = %ld; nstarts = %ld)\n", minspan, n,
    //        nstarts);
    for (long int i = 1; i < nstarts; i++) {
      long int span = starts[i] - starts[i - 1];
      if (span < minspan) {
        minspan = span;
        if (span < 0) {
          Rprintf("  something is wrong at i=%ld, where span (%ld) is negative",
                  i, minspan);
        }
      }
    }
    if (Length > minspan) {
      ::Rf_error("'length' (%ld) exceeds minimum span between 'starts' "
                 "elements (%ld); note: n=%ld, nstarts=%ld)",
                 Length, minspan, n, nstarts);
    }
  }
  for (long int i = 0; i < nstarts; i++) {
    long int off = Offset;
    for (long int j = 0; j < Length; j++) {
      res[k++] = starts[i] + off;
      off++;
      if (k > n) {
        break;
      }
    }
  }
  return res;
}
