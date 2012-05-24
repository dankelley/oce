#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define debug

/*
 * bisect_d (bisection search vector of doubles)
 * x = vector of doubles (must be in increasing order)
 * find = vector of doubles whose index to find
 * side = vector of 0 or 1 (to look to the left, or right, of find)
 * returns a vector of indices, or NA values
 *
 * TESTING:

 system("R CMD SHLIB misc.c") ; dyn.load("misc.so")

 x <- 0.1 + 1:1e7
 find <- c(10, 5e4)
 side <- c(0, 1)

 system.time({
 loc <- .Call("bisect_d", x, find, side);
 xx <- x[loc[1]:loc[2]]
 })

 user  system elapsed 
 0.021   0.000   0.021 

 system.time(xxx <- x[(find[1] < x) & (x < find[2])])

 user  system elapsed 
 0.266   0.001   0.264 

CONCLUSION: about 10 times faster than the straightforward method.
The latter might be an issue for deep use in loops, for large objects.

*/
SEXP bisect_d(SEXP x, SEXP find, SEXP side)
{
    PROTECT(x = AS_NUMERIC(x));
    double *px = NUMERIC_POINTER(x);
    PROTECT(find = AS_NUMERIC(find));
    double *pfind = NUMERIC_POINTER(find);
    PROTECT(side = AS_INTEGER(side));
    int *pside = INTEGER_POINTER(side);
    int nx = length(x);
    int nfind = length(find);
    int nside = length(side);
    if (nfind != nside)
        error("need length(find) = length(side)");
    int i;
    SEXP res;
    PROTECT(res = NEW_INTEGER(nfind));
    /* ensure that x is in order */
    for (i = 1; i < nx; i++) {
        if (px[i-1] >= px[i]) {
            char buf[1024];
            sprintf(buf, "x must be ordered from small to large; fails at x[%d]\n", i);
            error(buf);
        }
    }
    int *pres = INTEGER_POINTER(res);
    int left, right, middle, ifind;
    for (ifind = 0; ifind < nfind; ifind++) {
        double this_find = pfind[ifind];
#ifdef debug
        Rprintf("find[%d]=%f (%f <= x <= %f)\n", ifind, this_find, *(px), *(px+nx-1));
#endif
        /* trim indices to those of x (R notation) */
        if (this_find <= px[0]) {
            pres[ifind] = 1;
            continue;
        }
        if (this_find >= px[nx-1]) {
            pres[ifind] = nx;
            continue;
        }
        left = 0;
        right = nx - 1;
        int halves = (int)(10 + log(0.0+nx) / log(2.0)); /* prevent inf loop from poor coding */
        pres[ifind] = NA_INTEGER;
        for (int half = 0; half < halves; half++) {
            middle = (int)floor(0.5 * (left + right));
            /* exact match to middle? */
            if (px[middle] == pfind[ifind]) {
#ifdef debug
                Rprintf("exact match at middle=%d\n", middle);
#endif
                pres[ifind] = middle;
                break;
            }
            /* in left half */
            if (px[left] <= this_find & (this_find < px[middle])) {
#ifdef debug
                Rprintf("L %d %d\n", left, middle);
#endif
                right = middle;
                if (2 > (middle - left)) {
#ifdef debug
                    Rprintf("narrowed to left=%d and middle=%d\n", left, middle);
#endif
                    pres[ifind] = middle;
                    if (pside[ifind] == 0)
                        pres[ifind] = left + 1;
                    else
                        pres[ifind] = middle + 1;
                    break;
                }
            }
            /* in right half */
            if (px[middle] < this_find & (this_find <= px[right])) {
#ifdef debug
                Rprintf("R %d %d %f %f\n", middle, right, px[middle], px[right]);
#endif
                left = middle;
                if (2 > (right - middle)) {
#ifdef debug
                    Rprintf("narrowed to middle=%d and right=%d\n", middle, right);
                    Rprintf("pside=%d\n", pside[ifind]);
#endif
                    if (pside[ifind] == 0)
                        pres[ifind] = middle + 1;
                    else
                        pres[ifind] = right + 1;
#ifdef debug
                    Rprintf("pres[ifind]=%d\n",pres[ifind]);
#endif
                    break;
                }
            }
        }
    }
    UNPROTECT(4);
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
    if (!isMatrix(mat))
        error("'mat' must be a matrix");
    //if (isInteger(mat)) warning("'mat' is integer, but should be real");
    if (!isReal(mat))
        error("'mat' must be numeric, not integer");
    matp = REAL(mat);
    if (length(mat) != nrow * ncol)
        error("'nrow'*'ncol' must equal number of elements in 'mat'");
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

