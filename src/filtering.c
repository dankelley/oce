/* vim: set expandtab shiftwidth=2 softtabstop=2 tw=70: */

// NOTE: maybe I should just do this in R, with e.g.
// as.numeric(stats::filter(x=x, filter=c(.1, .8, .1), method="convolution"))
// and then fixing the endpoints.

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define DEBUG 1

/*

system("R CMD SHLIB filtering.c")
dyn.load("filtering.so");

#' Perform Hamming filtering
#'
#' Interior points are filtered using \link[stats]{filter} in the
#' \CRANpkg{stats} package for interior points, while leaving untouched
#' those points near the start and end of the vector.
#'
#' @param x a vector to be smoothed
#' @param n length of filter (must be an odd integer exceeding 1)
#'
#' @value a filtered version of \code{x}.
hammingFilter <- function(x, n)
{
    # .Call("hammingFilter", x, n)
    if (missing(x)) stop("must supply x")
    if (missing(n)) stop("must supply n")
    if (n < 1) stop("n must be be an integer exceeding 1")
    n2 <- n %/% 2 # half width
    if (2 * n2 == n) stop("n must be an odd integer")
    nx <- length(x)
    if (nx < n) return(x)
    twopi <- 8 * atan2(1, 1)
    f <- 0.54 - 0.46 * cos(twopi * (n2 + seq.int(-n2, n2, 1)) / (n - 1));
    f <- f / sum(f)
    rval <- as.numeric(stats::filter(x=x, filter=F, method="convolution"))
    start <- seq.int(1, n2)
    rval[start] <- x[start]
    end <- seq.int(nx-n2+1, nx)
    rval[end] <- x[end]
    rval
}

set.seed(123)
x <- seq(-6, 6) + rnorm(13, sd=0.5)
y <- hammingFilter(x, 5)
print(data.frame(x, y))
## next work in my testing, so I'm doing the formula right. But note
## that we need to normalize the filters ... why the heck are they
## not normalized in signal::hamming() or on wikipedia?
## signal::hamming(3) ## [1] 0.08 1.00 0.08
## signal::hamming(5) ## [1] 0.08 0.54 1.00 0.54 0.08

par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
plot(seq_along(x), x, type='b')
points(seq_along(x), y, col=2)
lines(seq_along(x), y, col=2)
grid()

*/


SEXP hammingFilter(SEXP x, SEXP n)
{
    PROTECT(x = AS_NUMERIC(x));
    PROTECT(n = AS_INTEGER(n));
    int nv = *INTEGER_POINTER(n);
    if (nv < 1 || !(nv % 2))
      error("n must be a positive odd integer, not %d", nv);
    double *xp = REAL(x);
    int nx = LENGTH(x);
    SEXP res;
    PROTECT(res = NEW_NUMERIC(nx));
    double *resp = REAL(res);
    double alpha = 0.54, beta = 0.46;

    // filter coefficients (R checks for allocation errors, and cleans
    // memory at the end of the .Call(), saving us some hassles. See
    // https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Memory-allocation
    double *f = (double *) R_alloc((size_t)nv, sizeof(double));
    for (unsigned long int i = 0; i < nv; i++) {
      f[i] = alpha - beta * cos(2 * M_PI * i / (nv - 1));
#ifdef DEBUG
      Rprintf("f[%d] = %f\n", i, f[i]);
#endif
    }
    // w(i) = alpha - beta * cos(2*pi*i/(n-1))
    int nv2 = 1 + (int)floor(nv/2);
    int trim = (int) floor(nv / 2.0); // e.g. 1, if nv=3
#ifdef DEBUG
    Rprintf("nv2=%d\n", nv2);
    Rprintf("trim=%d\n", trim);
#endif
    for (unsigned long int i = 0; i < nx; i++) {
      if (ISNA(xp[i]) || i < trim || (i > (nx - trim - 1))) {
        resp[i] = xp[i];
      } else {
        double tmp = 0.0, filter_sum = 0.0;
        for (int k = 0; k < nv; k++) {
          int ik = i + k - (nv2 - 1);
          if (ik < 0 || ik > (nx - 1))
            error("please report coding error: k=%d, ik=%d, nx=%d, nv=%d, nv2=%d\n", k, ik, nx, nv, nv2);
          tmp += f[k] * xp[ik];
#ifdef DEBUG
          Rprintf(" k=%d, ik=%d, f[k]=%f xp[ik]=%f so adding=%f\n", k, ik, f[k], xp[ik], f[k]*xp[ik]);
#endif
          filter_sum += f[k];
        }
        resp[i] = tmp / filter_sum;
      }
    }
    UNPROTECT(3);
    return(res);
}

