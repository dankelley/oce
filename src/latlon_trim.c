#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define debug

/*
  latlon_trim: change NA runs to single NA values

ARGUMENTS:
  x = vector of doubles
  y = vector of doubles
 
RETURN VALUE: vector of points to keep

TESTING:
x <- c(1,NA,3,NA,NA,6,NA,8,9)
y <- 1:9
system("R CMD SHLIB latlon_trim.c") ; dyn.load("latlon_trim.so"); keep<-.Call("latlon_trim", x, y)

*/
SEXP latlon_trim(SEXP x, SEXP y)
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(y = AS_NUMERIC(y));
    double *xp = NUMERIC_POINTER(x);
    double *yp = NUMERIC_POINTER(y);
    int i, nx=LENGTH(x), ny=LENGTH(y);
    if (nx != ny)
        error("in latlon_trim(): nx=%d must match ny=%d", nx);
    SEXP res;
    PROTECT(res = allocVector(LGLSXP, nx));
    int *resp = INTEGER(res);
    int last_NA = 0;
    for (i = 0; i < nx; i++) {
        if (0 == (i % 10))
            R_CheckUserInterrupt();
        if (ISNA(xp[i]) | ISNA(yp[i])) {
            // Rprintf("i=%d NA\n", i);
            if (last_NA)
                resp[i] = 0;
            else
                resp[i] = 1;
            last_NA = 1;
        } else {
            resp[i] = 1;
            last_NA = 0;
        }
    }
    UNPROTECT(3);
    return(AS_LOGICAL(res));
}

