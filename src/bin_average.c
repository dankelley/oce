#include <R.h>
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
void bin_average(int *nx, double *x, double *y, double *xmin, double *xmax, double *xinc, double *means)
{
    if (*nx < 1)
        error("invalid vector length (%d)", *nx);
    if (*xmin >= *xmax)
        error("cannot have xmin (%f) larger than or equal to xmax (%f)", *xmin, *xmax);
    if (*xinc <= 0)
        error("cannot have non-positive xinc (%f)", *xinc);
    // 'b' stands for bin
    int nb = (int)floor(((*xmax - *xmin) / *xinc));
    if (nb < 1)
        error("calculated number of regions (%d) is less than 1", nb);
    //Rprintf("nb=%d\n", nb);
    int *num = (int*)R_alloc(nb, sizeof(int));
    for (int b = 0; b < nb; b++) {
        num[b] = 0;
        means[b] = 0.0;
    }
    for (int i = 0; i < *nx; i++) {
        int b = (int)floor((x[i] - *xmin) / *xinc);
        //Rprintf("i=%d x=%f  y=%f  b=%d\n", i, x[i], y[i], b);
        if (-1 < b && b < nb) {
            num[b]++;
            means[b] += y[i];
            //Rprintf("  num %d   means %f\n", num[b], means[b]);
        }
    }
    for (int b = 0; b < nb; b++) {
        if (num[b] > 0)
            means[b] = means[b] / num[b];
    }
}
