#include <R.h>
#include <Rinternals.h>

/*
   system("R CMD SHLIB bin_average.c")
   x <- 1:100
   y <- 2 * x
   xbreaks <- seq(10, 50, 10)
   dyn.load("bin_average.so")
   a <- .C("bin_average", as.integer(length(x)), as.double(x), as.double(y),
   as.integer(length(xbreaks)), as.double(xbreaks), value=double(length(xbreaks)), NAOK=TRUE)#, PACKAGE="oce")
   plot(x, y);lines(xbreaks,a$value,type='s')
   */

void bin_average(int *nx, double *x, double *y, int *nbreaks, double *xbreaks, double *ymeans)
{
    if (*nx < 0)
        error("too few (%d) x values in bin_average", *nx);
    if (*nbreaks < 0)
        error("too few (%d) xbreak values in bin_average", *nbreaks);
    int *num = (int*)malloc((*nbreaks - 1) * sizeof(int));
    for (int ib = 0; ib < *nbreaks - 1; ib++) {
        Rprintf("ib %d has %f < x < %f\n", ib, xbreaks[ib], xbreaks[ib+1]);
        ymeans[ib] = 0.0;
        num[ib] = 0;
    }
    for (int i = 0; i < *nx; i++) {
        Rprintf("i %d x %f y %f\n", i, x[i], y[i]);
        for (int ib = 0; ib < *nbreaks - 1; ib++) {
            if (xbreaks[ib] <= x[i] && x[i] < xbreaks[ib+1]) {
                num[ib]++;
                ymeans[ib] += y[i];
                Rprintf("bin %d now has num %d and sum %f\n", ib, num[ib], ymeans[ib]);
                break;
            }
        }
    }
    for (int ib = 0; ib < *nbreaks - 1; ib++) {
        if (num[ib] != 0)
            ymeans[ib] = ymeans[ib] / num[ib];
    }
    free(num);
}
