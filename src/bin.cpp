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



/*
 

   library(oce)
   system("R CMD SHLIB bin.cpp")
   dyn.load("bin.so")
   set.seed(123)
   x <- rnorm(100, sd=1)
   y <- rnorm(100, sd=1)
   f <- x + y
   source('../R/misc.R')
   binMean2D(x, y, f)


*/



extern "C" {
    void bin_mean_2d(int *nx, double *x, double *y, double *f,
            int *nxbreaks, double *xbreaks,
            int *nybreaks, double *ybreaks,
            double *rval)
    {
        // array lookup
#define ij(i, j) ((i) + (*nxbreaks-1) * (j))
        Rprintf("nxbreaks: %d, nybreaks: %d\n", *nxbreaks, *nybreaks);
        if (*nxbreaks < 2) error("cannot have fewer than 1 xbreak"); // already checked in R but be safe
        if (*nybreaks < 2) error("cannot have fewer than 1 ybreak"); // already checked in R but be safe
        std::vector<double> bx(xbreaks, xbreaks + *nxbreaks);
        std::sort(bx.begin(), bx.end()); // STL wants breaks ordered
        std::vector<double> by(ybreaks, ybreaks + *nybreaks);
        std::sort(by.begin(), by.end()); // STL wants breaks ordered
        Rprintf("getting space for %d data\n", (*nxbreaks-1)*(*nybreaks-1));
        int *num = (int*)R_alloc((*nxbreaks-1)*(*nybreaks -1), sizeof(int)); // memory cleaned at return time
        for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
            //Rprintf("bij: %d, zero fill\n", bij);
            num[bij] = 0;
            rval[bij] = 0.0;
        }
        for (int i = 0; i < (*nx); i++) {
            if (!ISNA(f[i])) {
                // lower_bound is the the index of the smallest break exceeding x[i];
                // data above the top break yield index nbreak.
                int bi = std::lower_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
                int bj = std::lower_bound(by.begin(), by.end(), y[i]) - by.begin();
                if (bi > 0 && bj > 0 && bi < (*nxbreaks) && bj < (*nybreaks)) {
                    Rprintf("x: %6.3f, y: %6.3f, bi: %d, bj: %d\n", x[i], y[i], bi, bj);
                    num[ij(bi, bj)]++;
                    rval[ij(bi, bj)] += f[i];
                }
            }
        }
        for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
            if (num[bij] > 0) {
                rval[bij] = rval[bij] / num[bij];
            } else {
                rval[bij] = NA_REAL;
            }
        }
    }
}

