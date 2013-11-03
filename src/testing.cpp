#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <algorithm>
#include <vector>
//using namespace std;

/*
 
system("R CMD SHLIB testing.cpp")
dyn.load("testing.so")
x <- rnorm(10, sd=1)
y <- 2*x
breaks <- seq(-1, 1, 0.5)
junk <- .C("bin_mean_1d", length(x), as.double(x), as.double(y), length(breaks), as.double(breaks))

*/

extern "C" {
void bin_mean_1d(int *nx, double *x, double *y, int *nbreaks, double *breaks)
{

    std::vector<double> b(breaks, breaks + *nbreaks);
    std::sort (b.begin(), b.end()); // STL wants breaks ordered

    Rprintf("%d breaks:\n", *nbreaks);
    for (int i = 0; i < (*nbreaks); i++) {
        Rprintf("   %f\n", breaks[i]);
    }

    std::vector<double>::iterator lower_bound;
    for (int i = 0; i < (*nx); i++) {
        lower_bound = std::lower_bound(b.begin(), b.end(), x[i]); // smallest break exceeding x[i], or rightmost if too large
        int right_index = lower_bound - b.begin();
        if (right_index == *nbreaks) {
            Rprintf("x: %6.3f   right_index: %d    (outside range to right)\n", x[i], right_index);
        } else if (right_index == 0) {
            Rprintf("x: %6.3f   right_index: %d    (outside range to left)\n", x[i], right_index);
        } else {
            Rprintf("x: %6.3f   right_index: %d    (%f to %f)\n", x[i], right_index, breaks[right_index-1], breaks[right_index]);
        }
    }
#if 0
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
#endif
}
}
