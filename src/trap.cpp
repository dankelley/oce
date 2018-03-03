#include <Rcpp.h>
using namespace Rcpp;

// This is a utility function called by integrateTrapeziod(),
// so it does not get a wrapper inserted into the R namespace;
// that's why there is no roxygen @export here, and no roxygen
// documentation, either. However, we need to tell Rcpp to export
// it, so integreateTrapezoid() can .Call() it.
//
// [[Rcpp::export]]
NumericVector trap(NumericVector x, NumericVector y, NumericVector type)
{
  int type_value = int(floor(0.5 + type[0]));
  int n = x.size();
  if ((n > 1) && n != y.size())
    ::Rf_error("lengths of x (%d) and y (%d) do not match", n, y.size());
  if (0 == type_value) { // area
    NumericVector res(1);
    res[0] = 0.0;
    for (int i = 1; i < n; i++)
      res[0] += 0.5 * (y[i] + y[i-1]) * (x[i] - x[i-1]);
    return(res);
  } else if (1 == type_value) { // area elements
    NumericVector res(n);
    res[0] = 0.0;
    for (int i = 1; i < n; i++)
      res[i] = 0.5 * (y[i] + y[i-1]) * (x[i] - x[i-1]);
    return(res);
  } else if (2 == type_value) { // cumulative area elements
    NumericVector res(n);
    res[0] = 0.0;
    for (int i = 1; i < n; i++)
      res[i] = res[i-1] + 0.5 * (y[i] + y[i-1]) * (x[i] - x[i-1]);
    return(res);
  } else {
    NumericVector res(1);
    res[0] = NA_REAL;
    ::Rf_error("unknown type %d; must be 0, 1, or 2\n", type_value);
    return(res);
  }
}

