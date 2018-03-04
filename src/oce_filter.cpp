/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <Rcpp.h>
using namespace Rcpp;

// This is a utility function, so its auto-generated wrapper is not put into
// the R namespace; that's why there is no roxygen @export here, and no roxygen
// documentation, either.
//
// NOTE: update src/registerDynamicSymbol.c for this!
//
// [[Rcpp::export]]
NumericVector do_oce_filter(NumericVector x, NumericVector a, NumericVector b)
{
  int na = a.size();
  int nb = b.size();
  int nx = x.size();
  NumericVector y(nx);
  for (int i = 0; i < nx; i++) {
    double xsum, ysum;
    int ioffset; /* prevent looking before start */
    xsum = 0.0;
    for (int ib = 0; ib < nb; ib++) {
      ioffset = i - ib;
      if (ioffset > -1)
        xsum += b[ib] * x[ioffset];
    }
    ysum = 0.0;
    for (int ia = 1; ia < na; ia++) {
      ioffset = i - ia;
      if (ioffset > -1)
        ysum += a[ia] * y[ioffset];
    }
    y[i] = xsum - ysum;
  }
  return(y);
}
