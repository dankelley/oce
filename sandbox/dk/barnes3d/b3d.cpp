#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double b3d(NumericVector v, IntegerVector dim)
{
    double res;
    res = v(1);
    Rcout << dim << "\n";
    return(res);
}

