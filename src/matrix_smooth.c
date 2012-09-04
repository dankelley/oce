#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

SEXP matrix_smooth(SEXP mat)
{
#define ij(i, j) ((i) + (ni) * (j))
    /* Note: the 2d data are stored in column order */
    SEXP res;
    int ni = INTEGER(GET_DIM(mat))[0];
    int nj = INTEGER(GET_DIM(mat))[1];
    int i, j;
    double *matp, *resp;
    if (!isMatrix(mat))
        error("'mat' must be a matrix");
    if (!isReal(mat))
        error("'mat' must be numeric, not integer");
    matp = REAL(mat);
    if (length(mat) != ni * nj)
        error("'ni'*'nj' must equal number of elements in 'mat'");
    PROTECT(res = allocMatrix(REALSXP, ni, nj));
    resp = REAL(res);
    for (i = 0; i < ni*nj; i++)
        resp[i] = 99.99;
    // copy edges (FIXME: coiuld use 1D smoother here)
    for (j = 0; j < nj; j++) {
        resp[ij(0, j)] = matp[ij(0, j)];
        resp[ij(ni-1, j)] = matp[ij(ni-1, j)];
    }
    for (i = 0; i < ni; i++) {
        resp[ij(i, 0)] = matp[ij(i, 0)];
        resp[ij(i, nj-1)] = matp[ij(i, nj-1)];
    }
    // smooth middle 
    for (i = 1; i < ni - 1; i++)
        for (j = 1; j < nj - 1; j++)
            resp[ij(i, j)] = (2.0*matp[ij(i, j)] +
                    matp[ij(i-1, j)] +
                    matp[ij(i+1, j)] +
                    matp[ij(i, j-1)] +
                    matp[ij(i, j+1)]) / 6.0;
    UNPROTECT(1);
    return(res);
#undef ix
}

