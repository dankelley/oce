#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define debug

/*
 * oce_convolve -- convolution without oddness of stats:filter() or convolve()
 *
 * TESTING:

 system("R CMD SHLIB oce_convolve.c") ; dyn.load("oce_convolve.so")
 source('../R/misc.R')
 dyn.load("oce_convolve.so")
 t <- 0:1027
 tau <- 10
 n <- length(t)
 signal <- ifelse(sin(t * 2 * pi / 128) > 0, 1, 0)
 filter <- exp(-seq(5*tau, 0) / tau)
 filter <- filter / sum(filter)
 obs <- oce.convolve(signal, filter)
 plot(t, signal, type='l', lwd=4)
 lines(t, obs, col='red')

*/
SEXP oce_convolve(SEXP x, SEXP f, SEXP end)
{
    PROTECT(x = AS_NUMERIC(x));
    double *xp = NUMERIC_POINTER(x);
    PROTECT(f = AS_NUMERIC(f));
    double *fp = NUMERIC_POINTER(f);
    PROTECT(end = AS_NUMERIC(end));
    double *endp = NUMERIC_POINTER(end);
    int endflag = floor(0.5 + (*endp));
    int nx = length(x);
    int nf = length(f);
    int i, ij, j;
    SEXP res;
    PROTECT(res = NEW_NUMERIC(nx));
    double *resp = NUMERIC_POINTER(res);
    // do not look past the start of x
    // Just copy x, until we get to the point where the filter will fit
    if (endflag == 0) {
        for (i = 0; i < nf; i++)
            resp[i] = 0.0;
        for (i = nf; i < nx; i++) {
            resp[i] = 0.0;
            for (j = 0; j < nf; j++) {
                resp[i] += fp[j] * xp[i - j];
            }
        }

    } else if (endflag == 1) {
        for (i = 0; i < nf; i++)
            resp[i] = xp[i];
        for (i = nf; i < nx; i++) {
            resp[i] = 0.0;
            for (j = 0; j < nf; j++) {
                resp[i] += fp[j] * xp[i - j];
            }
        }

    } else if (endflag == 2) {
        for (i = nf; i < nx; i++) {
            resp[i] = 0.0;
            for (j = 0; j < nf; j++) {
                ij = i - j;
                if (ij >= 0)
                    resp[i] += fp[j] * xp[ij];
            }
        }
    } else {
        error("'end' must be 0, 1, or 2");
    }
    for (i = 0; i < nf; i++)
        resp[i] = xp[i];
    for (i = nf; i < nx; i++) {
        resp[i] = 0.0;
        for (j = 0; j < nf; j++) {
            resp[i] += fp[j] * xp[i - j];
        }
    }
    UNPROTECT(4);
    return(res);
}

