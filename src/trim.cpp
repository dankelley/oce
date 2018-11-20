/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <Rcpp.h>
using namespace Rcpp;

// Find start and stop indices in x that enclose xlim with
// one extra element less than xlim[1] and one more than xlim[2].
/*

   library(Rcpp)
   sourceCpp("trim.cpp")
   x <- seq(0, 10, 0.1)
   xlim <- c(2.0, 2.9)
   expect_equal(trim_ts(x, xlim, 0), list(from=20, to=31))

*/


// [[Rcpp::export]]
List trim_ts(NumericVector x, NumericVector xlim, NumericVector extra)
{
  int nx = x.size();
  int nxlim = xlim.size();
  if (nxlim != 2)
    ::Rf_error("In trim_ts(), length of xlim must be 2 but it is %d\n", nxlim);
  if (xlim[1] < xlim[0])
    ::Rf_error("In trim_ts(), xlim must be ordered but it is (%g, %g)\n", xlim[0], xlim[1]);
  for (int i = 1; i < nx; i++) {
    if (x[i] < x[i-1]) {
      ::Rf_error("In trim_ts(), x must be ordered but x[%d]=%.10g and x[%d]=%.10g\n",
          i-1, x[i-1], i, x[i]);
    }
  }
  double epsilon = (x[1] - x[0]) / 1e9;

  double start = xlim[0] - extra[0]*(xlim[1]-xlim[0]) - epsilon;
  double end = xlim[1] + extra[0]*(xlim[1]-xlim[0]) + epsilon;

  NumericVector from(1), to(1);
  for (int i = 0; i < nx; i++) {
    if (x[i] >= start) {
      from[0] = (double)i;
      break;
    }
  }
  for (int i = nx-1; i >= 0; i--) {
    if (x[i] < end) {
      to[0] = (double)i+2;
      break;
    }
  }
  if (from[0] < 1.0) from[0] = 1.0;
  if (to[0] > nx) to[0] = (double)nx;
  return(List::create(Named("from")=from, Named("to")=to));
}

