#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <algorithm>
#include <vector>

//#define DEBUG

/*


   library(oce)
   system("R CMD SHLIB testing.cpp")
   dyn.load("testing.so")
   set.seed(123)
   x <- rnorm(10, sd=1)
   y <- 2*x
   breaks <- seq(-1, 1, 0.5)
   nbreaks <- length(breaks)
   test <- .C("bin_mean_1d", length(x), as.double(x), as.double(y),
   length(breaks), breaks=as.double(breaks), rval=double(nbreaks-1))
   mids <- breaks[-1] - 0.5 * diff(breaks)
   old <- binAverage(x, y, -1, 1, 0.5)
   data.frame(mids=mids, mean=test$rval, oldMethod=old$y)


*/

extern "C" {
    void bin_mean_1d(int *nx, double *x, double *f, int *nbreaks, double *breaks, double *rval)
    {

        if (*nbreaks < 2)
            error("cannot have fewer than 1 break"); // already checked in R but be safe
        std::vector<double> b(breaks, breaks + *nbreaks);
        std::sort(b.begin(), b.end()); // STL wants breaks ordered
#ifdef DEBUG
        Rprintf("%d breaks:\n", *nbreaks);
        for (int i = 0; i < (*nbreaks); i++) {
            Rprintf("   %f\n", breaks[i]);
        }
#endif
        int *num = (int*)R_alloc(*nbreaks-1, sizeof(int)); // memory cleaned at return time
        for (int i = 0; i < (*nbreaks); i++) {
            num[i] = 0;
            rval[i] = 0.0;
        }
        for (int i = 0; i < (*nx); i++) {
            if (!ISNA(f[i])) {
                std::vector<double>::iterator lower_bound;
                // lower_bound is the the index of the smallest break exceeding x[i];
                // data above the top break yield index nbreak.
                lower_bound = std::lower_bound(b.begin(), b.end(), x[i]);
                int ii = lower_bound - b.begin();
                if (ii == *nbreaks) {
#ifdef DEBUG
                    Rprintf("x: %6.3f   ii: %d    >>>\n", x[i], ii);
#endif
                } else if (ii == 0) {
#ifdef DEBUG
                    Rprintf("x: %6.3f   ii: %d    <<<\n", x[i], ii);
#endif
                } else {
#ifdef DEBUG
                    Rprintf("x: %6.3f   ii: %d    (%f to %f)\n",
                            x[i], ii, breaks[ii-1], breaks[ii]);
#endif
                    num[ii-1]++;
                    rval[ii-1] += f[i];
                }
            }
        }
        for (int i = 0; i < (*nbreaks); i++) {
            if (num[i] > 0) {
                rval[i] = rval[i] / num[i];
            } else {
                rval[i] = NA_REAL;
            }
        }
    }
}
