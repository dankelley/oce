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
    int nx = length(x), ny = length(y);
    if ((nx > 1) && nx != ny)
        error("lengths of x (%d) and y (%d) must match", nx, ny);
    double *xp = REAL(x), *yp = REAL(y);
    double dx = 1.0;
    if (nx == 1)
        dx = *xp;
    int *typep = INTEGER(type);
    SEXP res;
    double *resp = NULL;
    switch(*typep) {
        case 0: // area
            PROTECT(res = NEW_NUMERIC(1));
            resp = REAL(res);
            *resp = 0.0;
            for (int i = 1; i < ny; i++) {
                if (nx != 1)
                    dx = (xp[i] - xp[i-1]);
                *resp += 0.5 * (yp[i] + yp[i-1]) * dx;
            }
            break;
        case 1: // area elements
            PROTECT(res = NEW_NUMERIC(ny));
            resp = REAL(res);
            resp[0] = 0.0;
            for (int i = 1; i < ny; i++) {
                if (nx != 1)
                    dx = (xp[i] - xp[i-1]);
                resp[i] = 0.5 * (yp[i] + yp[i-1]) * dx;
            }
            break;
        case 2: // cumulative area elements
            PROTECT(res = NEW_NUMERIC(ny));
            resp = REAL(res);
            resp[0] = 0.0;
            for (int i = 1; i < ny; i++) {
                if (nx != 1)
                    dx = (xp[i] - xp[i-1]);
                resp[i] = resp[i-1] + 0.5 * (yp[i] + yp[i-1]) * dx;
            }
            break;
        default:
            PROTECT(res = NEW_NUMERIC(1));
            resp[0] = 0.0;
            error("unknown type %d; must be 0, 1, or 2\n", *typep);
    }
    UNPROTECT(4);
    return(res);
}

