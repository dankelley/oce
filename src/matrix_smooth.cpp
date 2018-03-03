#include <Rcpp.h>
using namespace Rcpp;

// This is a utility function called by matrixSmooth,
// so it does not get a wrapper inserted into the R namespace;
// that's why there is no roxygen @export here, and no roxygen
// documentation, either. However, we need to tell Rcpp to export
// it, so matrixSmooth() can .Call() it.
//
// [[Rcpp::export]]

NumericMatrix matrix_smooth(NumericMatrix mat)
{
    int ni = mat.nrow(), nj=mat.ncol();
    NumericMatrix res(ni, nj);
    // copy edges (FIXME: could use 1D smoother here)
    for (int j = 0; j < nj; j++) {
        res(0, j) = mat(0, j);
        res(ni-1, j) = mat(ni-1, j);
    }
    for (int i = 0; i < ni; i++) {
        res(i, 0) = mat(i, 0);
        res(i, nj-1) = mat(i, nj-1);
    }
    // smooth middle 
    for (int i = 1; i < ni - 1; i++)
        for (int j = 1; j < nj - 1; j++)
            res(i, j) = (2.0*mat(i, j) +
                    mat(i-1, j) + mat(i+1, j) + mat(i, j-1) + mat(i, j+1)) / 6.0;
    return(res);
}

