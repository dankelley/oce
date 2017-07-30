/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
   system("R CMD SHLIB trap.c")
   x <- seq(0, 1, 0.1)
   y <- 2*x + 3*x^2
   dyn.load("trap.so")
   print(.Call("trap", x, y, "A"))
   print(.Call("trap", x, y, TRUE))
   */

SEXP trap(SEXP x, SEXP y, SEXP type)
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    PROTECT(type = AS_INTEGER(type));
    int type_value = *INTEGER_POINTER(type);
    int nx = length(x), ny = length(y);
    if ((nx > 1) && nx != ny)
        error("lengths of x (%d) and y (%d) must match", nx, ny);
    double *xp = REAL(x), *yp = REAL(y);
    double dx = 1.0;
    if (nx == 1)
        dx = *xp;
    SEXP res;
    //Rprintf("nx=%d, ny=%d\n", nx, ny);
    double *resp;
    if (0 == type_value) { // area
        PROTECT(res = allocVector(REALSXP, 1));
        resp = REAL(res);
        resp[0] = 0.0;
        for (int i = 1; i < ny; i++) {
            if (nx != 1)
                dx = (xp[i] - xp[i-1]);
            resp[0] += 0.5 * (yp[i] + yp[i-1]) * dx;
        }
    } else if (1 == type_value) { // area elements
        PROTECT(res = allocVector(REALSXP, ny));
        resp = REAL(res);
        resp[0] = 0.0;
        for (int i = 1; i < ny; i++) {
            if (nx != 1)
                dx = (xp[i] - xp[i-1]);
            resp[i] = 0.5 * (yp[i] + yp[i-1]) * dx;
        }
    } else if (2 == type_value) { // cumulative area elements
        //Rprintf("trap() type=%d\n", type_value);
        PROTECT(res = allocVector(REALSXP, ny));
        resp = REAL(res);
        resp[0] = 0.0;
        //Rprintf("i=%d x=%f y=%f res=%f\n", 0, xp[0], yp[0], resp[0]);
        for (int i = 1; i < ny; i++) {
            if (nx != 1)
                dx = (xp[i] - xp[i-1]);
            resp[i] = resp[i-1] + 0.5 * (yp[i] + yp[i-1]) * dx;
            //if (i < 10) Rprintf("i=%d x=%f y=%f res=%f\n", i, xp[i], yp[i], resp[i]);
        }
    } else {
        PROTECT(res = allocVector(REALSXP, 1));
        resp = REAL(res);
        resp[0] = 0.0;
        error("unknown type %d; must be 0, 1, or 2\n", type_value);
    }
    UNPROTECT(4);
    return(res);
}

