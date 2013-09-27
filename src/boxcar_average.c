#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
/*
 
setwd('~/src/R-kelley/oce/src')
system("R CMD SHLIB boxcar_average.c")
dyn.load("boxcar_average.so")
x <- seq(0, 10, length.out=100)
y <- 1 + x + x*x
source('~/src/R-kelley/oce/R/misc.R')
plot(x, y)
ba <- boxcarAverage(x, y, seq(0, 10, 2))
points(ba$xout, ba$average, col='red')

*/


SEXP boxcar_average(SEXP x, SEXP y, SEXP xout)
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(xout = AS_NUMERIC(xout));
    double *xp = REAL(x);
    double *yp = REAL(y);
    double *xoutp = REAL(xout);
    int nx = LENGTH(x);
    int ny = LENGTH(y);
    int nxout = LENGTH(xout);

    if (!IS_VECTOR(x)) {
        int xRank = LENGTH(GET_DIM(x));
        Rprintf("xRank: %d\n", xRank);
        Rprintf("should not be able to get here in code; see src/boxcar_average.c and R/misc.R[boxcarAverage]\n");
    }
    if (!IS_VECTOR(y)) {
        int yRank = LENGTH(GET_DIM(y));
        Rprintf("yRank: %d\n", yRank);
        Rprintf("should not be able to yet here in code; see src/boxcar_averaye.c and R/misc.R[boxcarAveraye]\n");
    }
    if (!IS_VECTOR(xout)) {
        int xoutRank = LENGTH(GET_DIM(xout));
        Rprintf("xoutRank: %d\n", xoutRank);
        Rprintf("should not be able to xoutet here in code; see src/boxcar_averaxoute.c and R/misc.R[boxcarAveraxoute]\n");
    }

    SEXP avg; // first holds sum, then (divided by count), the average
    PROTECT(avg = allocVector(REALSXP, nxout));
    double *avgp = REAL(avg);
    SEXP count;
    PROTECT(count = allocVector(REALSXP, nxout));
    double *countp = REAL(count);
    for (int i = 0; i < nxout; i++) {
        avgp[i] = 0.0;
        countp[i] = 0.0;
    }
    // FIXME: what if dg < 0 or dg == 0?  (check in R code)
    double xoutMin = xoutp[0];
    double xoutMax = xoutp[nxout-1];
    double xoutInc = xoutp[1] - xoutp[0];
    for (int i=0; i < nx; i++) {
        // FIXME: assuming regular grid
        int which = (int)floor(0.5 + (xp[i] - xoutMin) / xoutInc);
        if (0 < which && which < nxout) {
            avgp[which] += yp[i];
            countp[which] += 1.0;
        } else {
        }
    }
    for (int i=0; i < nxout; i++) {
        if (countp[i] > 0.0) {
        }
    }
    // create return value, a list
    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 2));
    PROTECT(res_names = allocVector(STRSXP, 2));
    SET_VECTOR_ELT(res, 0, avg);
    SET_STRING_ELT(res_names, 0, mkChar("average"));
    SET_VECTOR_ELT(res, 1, xout);
    SET_STRING_ELT(res_names, 1, mkChar("xout"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(7);
    return(res);
}

