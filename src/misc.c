#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
 * bisect_d (bisection search vector of doubles)
 * x = vector of doubles (must be in increasing order)
 * f = vector of doubles whose index to find
 * returns a vector of indices, or NA values
 *
 * TESTING:

 system("R CMD SHLIB misc.c")
 dyn.load("misc.so")
 .Call("bisect_d", 1:1e8, c(10,5e4,33))

 * SPEED: above takes 0.842s; below, more direct, is 3 times slower:
 * system.time({i<-(1:1e8)<1e4;j<-(1:1e8)>1e3})
 *  user  system elapsed 
 * 2.469   2.735   9.108 
 * 

*/
SEXP bisect_d(SEXP x, SEXP find)
{
    PROTECT(x = AS_NUMERIC(x));
    double *px = NUMERIC_POINTER(x);
    PROTECT(find = AS_NUMERIC(find));
    double *pfind = NUMERIC_POINTER(find);
    int nx = length(x);
    int nfind = length(find);
    char buf[1024]; /* for error messages */
    int i;
    SEXP res;
    PROTECT(res = NEW_INTEGER(nfind));
    /* ensure that x is in order */
    for (i = 1; i < nx; i++) {
        if (px[i-1] >= px[i]) {
            char buf[256];
            sprintf(buf, "x must be ordered from small to large; fails at x[%d]\n", i);
            error(buf);
        }
    }
    int *pres = INTEGER_POINTER(res);
    int left, right, middle, ifind;
    for (ifind = 0; ifind < nfind; ifind++) {
        double this_find = *(pfind + ifind);
        //Rprintf("find[%d]=%f (%f <= x <= %f)\n", ifind, this_find, *(px), *(px+nx-1));
        /* trim indices to those of x (R notation) */
        if (this_find <= *(px)) {
            pres[ifind] = 1;
            continue;
        }
        if (this_find >= *(px+nx-1)) {
            pres[ifind] = nx;
            continue;
        }
        int p;
        left = 0;
        right = nx - 1;
        int halves = (int)(3 + log(0.0+nx) / log(2.0)); /* prevent inf loop from poor coding */
        pres[ifind] = NA_INTEGER;
        for (int half = 0; half < halves; half++) {
            middle = (int)floor(0.5 * (left + right));
            /* exact match to middle? */
            if (*(px + middle) == pfind[ifind]) {
                //Rprintf("exact match at middle=%d\n", middle);
                pres[ifind] = middle - 1;
                break;
            }
            /* in left half */
            if (px[left] <= this_find & this_find <= px[middle]) {
                //Rprintf("LEFT %d - %d\n", left, middle);
                right = middle;
                if (2 > (middle - left)) {
                    //Rprintf("narrowed to left=%d and middle=%d\n", left, middle);
                    pres[ifind] = middle - 1;
                    break;
                }
            }
            /* in right half */
            if (px[middle] <= this_find & this_find <= px[right]) {
                //Rprintf("RIGHT %d - %d\n", middle, right);
                left = middle;
                if (2 > (right - middle)) {
                    //Rprintf("narrowed to middle=%d and right=%d\n", middle, right);
                    pres[ifind] = middle - 1;
                    break;
                }
            }
        }
    }
    UNPROTECT(3);
    return(res);
}

SEXP matrix_smooth(SEXP mat)
{
    /* Note: the 2d data are stored in column order */
    SEXP res;
    int nrow = INTEGER(GET_DIM(mat))[0];
    int ncol = INTEGER(GET_DIM(mat))[1];
    int i, j;
    double *matp, *resp;
    if (!isMatrix(mat)) error("'mat' must be a matrix");
    //if (isInteger(mat)) warning("'mat' is integer, but should be real");
    if (!isReal(mat)) error("'mat' must be numeric, not integer");
    matp = REAL(mat);
    if (length(mat) != nrow * ncol) error("'nrow'*'ncol' must equal number of elements in 'mat'");
    PROTECT(res = allocMatrix(REALSXP, nrow, ncol));
    resp = REAL(res);
    // copy edges (change this, if filter size changes)
    for (j = 0; j < ncol; j++) {
        *(resp + j                    ) = *(matp + j                    );
        *(resp + j + ncol * (nrow - 1)) = *(matp + j + ncol * (nrow - 1));
    }
    for (i = 0; i < nrow; i++) {
        *(resp +      0     + ncol * i) = *(matp +      0     + ncol * i);
        *(resp + (nrow - 1) + ncol * i) = *(matp + (nrow - 1) + ncol * i);
    }
    // smooth middle 
    for (i = 1; i < nrow - 1; i++) {
        for (j = 1; j < ncol - 1; j++) {
            *(resp + j + ncol * i) = 
                (2*(*(matp + j     + ncol *    i    )) +
                 (  *(matp + j - 1 + ncol *    i    )) +
                 (  *(matp + j + 1 + ncol *    i    )) +
                 (  *(matp + j     + ncol * (i - 1) )) +
                 (  *(matp + j     + ncol * (i + 1) ))) / 6.0;
        }
    }
    UNPROTECT(1);
    return(res);
}
#undef SQR

