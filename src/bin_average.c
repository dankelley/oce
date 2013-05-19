#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/*
   system("R CMD SHLIB bin_average.c")
   x <- 1:60 + 0.5
   y <- 2 * x
   dyn.load("bin_average.so")
   a <- .C("bin_average", length(x), as.double(x), as.double(y), 10, 50, 10, means=double(4), NAOK=TRUE)#, PACKAGE="oce")
   plot(x, y)
   points(5 + seq(10, 40, 10), a$means, col='red', cex=3)
   lines(5 + seq(10, 40, 10), a$means, col='red')
   abline(v=seq(10, 50, 10))
   */

// CAUTION: calling code must allocate 'means' of length floor((xmax-xmin)/xinc)

//#define DEBUG

void bin_average(int *nx, double *x, double *y, double *xmin, double *xmax, double *xinc, double *means)
{
    if (*nx < 1)
        error("invalid vector length (%d)", *nx);
    if (*xmin >= *xmax)
        error("xmin (%f) may not exceed xmax (%f)", *xmin, *xmax);
    if (*xinc <= 0)
        error("cannot have non-positive xinc (%f)", *xinc);
    // 'b' stands for bin
    //int nb = (int)floor(((*xmax - *xmin) / *xinc));
    int nb = (int)floor(((*xmax - *xmin) / *xinc));
    if (nb < 1)
        error("calculated number of regions (%d) is less than 1", nb);
#ifdef DEBUG
    Rprintf("xmin=%f  xmax=%f  xinc=%f  so calculate nb=%d\n", *xmin, *xmax, *xinc, nb);
#endif
    int *num = (int*)R_alloc(nb, sizeof(int)); // R will clean up memory after .C() returns
    for (int b = 0; b < nb; b++) {
        num[b] = 0;
        means[b] = 0.0;
    }
    int b;
    for (int i = 0; i < *nx; i++) {
        if (ISNA(y[i]))
            continue;
        b = (int)floor((x[i] - *xmin) / (*xinc));
#ifdef DEBUG
        Rprintf("  x[%d]=%f yields b=%d (%.20f) (%.20f)", i, x[i], b, x[i]-*xmin, (x[i]-*xmin)/ *xinc);
#endif
        if (-1 < b && b < nb) {
            num[b]++;
            means[b] +=y[i];
#ifdef DEBUG
            Rprintf(" inside  range, so use  y[%d]=%f; now, num[b]=%d means[b]=%f\n", i, y[i], num[b], means[b]);
#endif
        } else if (b == nb && x[i] == *xinc * b) { // catch exact match to max value; min value works without a trick
            num[b-1]++;
            means[b-1] += y[i];
#ifdef DEBUG
            Rprintf(" exact match to large end of range, so use y[%d]=%f; now, num[%d]=%d means[b]=%f\n", i, y[i], b-1, num[b-1], means[b-1]);
#endif
        } else {
#ifdef DEBUG
            Rprintf(" outside range, so skip y[%d]=%f <%.20f>\n", i, y[i], *xinc * b - x[i]);
#endif
        }
    }
    for (int b = 0; b < nb; b++) {
        if (num[b] > 0)
            means[b] = means[b] / num[b];
        else
            means[b] = NA_REAL;
    }
}
