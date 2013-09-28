#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG

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


SEXP boxcar_average_vector(SEXP x, SEXP y, SEXP xout)
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
        if (0 <= which && which < nxout) {
            avgp[which] += yp[i];
            countp[which] += 1.0;
        } else {
        }
    }
    for (int i=0; i < nxout; i++) {
        if (countp[i] > 0.0) {
            avgp[i] /= countp[i];
        } else {
            avgp[i] = NA_REAL;
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


/*

setwd('~/src/R-kelley/oce/src')
system("R CMD SHLIB boxcar_average.c")
dyn.load("boxcar_average.so")
source('~/src/R-kelley/oce/R/misc.R')
x <- runif(1000)
y <- runif(1000)
z <- x - y
plot(x, y, pch=20, col='gray')
ba <- boxcarAverage2D(x, y, z, seq(0, 1, 0.2), seq(0, 1, 0.2))
contour(ba$x1out, ba$x2out, ba$average, add=TRUE, labcex=1)

*/


SEXP boxcar_average_2d(SEXP x1, SEXP x2, SEXP y, SEXP x1out, SEXP x2out)
{

    // array lookup
#define ij(i, j) ((i) + (nx1out) * (j))
    PROTECT(x1 = AS_NUMERIC(x1));
    PROTECT(x2 = AS_NUMERIC(x2));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(x1out = AS_NUMERIC(x1out));
    PROTECT(x2out = AS_NUMERIC(x2out));
    double *x1p = REAL(x1);
    double *x2p = REAL(x2);
    double *yp = REAL(y);
    double *x1outp = REAL(x1out);
    double *x2outp = REAL(x2out);
    int nx1out = LENGTH(x1out);
    int nx2out = LENGTH(x2out);
#ifdef DEBUG
    Rprintf("output array will have dimension %d x %d\n", nx1out, nx2out);
#endif

    SEXP avg; // first holds sum, then (divided by count), the average
    PROTECT(avg = allocMatrix(REALSXP, nx1out, nx2out));
    double *avgp = REAL(avg);
    SEXP count;
    PROTECT(count = allocMatrix(REALSXP, nx1out, nx2out));
    double *countp = REAL(count);
    for (int ij = 0; ij < nx1out * nx2out; ij++) {
        avgp[ij] = 0.0;
        countp[ij] = 0.0;
    }
    // FIXME: what if dg < 0 or dg == 0?  (check in R code)
    double x1outMin = x1outp[0];
    double x2outMin = x2outp[0];
    double x1outMax = x1outp[nx1out-1];
    double x2outMax = x2outp[nx2out-1];
    double x1outInc = x1outp[1] - x1outp[0];
    double x2outInc = x2outp[1] - x2outp[0];
    int nx = LENGTH(x1);
#ifdef DEBUG
    Rprintf("nx=%d (should be 31)\n", nx);
#endif
    for (int i=0; i < nx; i++) {
        // FIXME: assuming regular grid
        int which1 = (int)floor(0.5 + (x1p[i] - x1outMin) / x1outInc);
        int which2 = (int)floor(0.5 + (x2p[i] - x2outMin) / x2outInc);
#ifdef DEBUG
        Rprintf("x[%d]=%f y[%d]=%f -> [%d, %d]\n", i, x1p[i], i, x2p[i], which1, which2);
#endif
        if (0 <= which1 && which1 < nx1out && 0 <= which2 && which2 < nx2out) {
            avgp[ij(which1, which2)] += yp[i];
            countp[ij(which1, which2)] += 1.0;
        }
    }
    for (int i=0; i < nx1out; i++) {
        for (int j=0; j < nx2out; j++) {
            if (countp[ij(i, j)] > 0.0) {
                avgp[ij(i, j)] /= countp[ij(i, j)];
            } else {
                avgp[ij(i, j)] = NA_REAL;
            }
        }
    }
    // create return value, a list
    SEXP res;
    SEXP res_names;
    PROTECT(res = allocVector(VECSXP, 3));
    PROTECT(res_names = allocVector(STRSXP, 3));
    SET_VECTOR_ELT(res, 0, avg);
    SET_STRING_ELT(res_names, 0, mkChar("average"));
    SET_VECTOR_ELT(res, 1, x1out);
    SET_STRING_ELT(res_names, 1, mkChar("x1out"));
    SET_VECTOR_ELT(res, 2, x2out);
    SET_STRING_ELT(res_names, 2, mkChar("x2out"));
    setAttrib(res, R_NamesSymbol, res_names);
    UNPROTECT(9);
#undef ij
    return(res);
}

