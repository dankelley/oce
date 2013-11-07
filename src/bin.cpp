#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <algorithm>
#include <vector>

//#define DEBUG

/*


   library(oce)
   system("R CMD SHLIB bin.cpp")
   dyn.load("bin.so")
   set.seed(123)
   x <- rnorm(10, sd=1)
   f <- 2*x
   source('../R/misc.R')
   m <- binMean1D(x, f, seq(-1.5, 1.5, 0.5))
   old <- binAverage(x, f, -1.5, 1.5, 0.5)
   data.frame(mids=m$xmids, mean=m$mean, oldMethod=old$y)


*/

extern "C" {
    void bin_mean_1d(int *nx, double *x, double *f, int *nbreaks, double *breaks,
            int *number, double *mean)
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
        for (int i = 0; i < (*nbreaks); i++) {
            number[i] = 0;
            mean[i] = 0.0;
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
                    number[ii-1]++;
                    mean[ii-1] += f[i];
                }
            }
        }
        for (int i = 0; i < (*nbreaks-1); i++) {
            if (number[i] > 0) {
                mean[i] = mean[i] / number[i];
            } else {
                mean[i] = NA_REAL;
            }
        }
    }
}



/*
 

   library(oce)
   system("R CMD SHLIB bin.cpp") # must be in oce/src
   dyn.load("bin.so")
   set.seed(123)
   x <- rnorm(100, sd=1)
   y <- rnorm(100, sd=1)
   f <- x + y
   source('../R/misc.R')
   m <- binMean2D(x, y, f)
   a <- boxcarAverage2D(x, y, f, m$xmids, m$ymids)
   cat("mismatch to old code:", sum((a$average-m$mean)^2, na.rm=TRUE), "\n")
   str(m)
   str(a)
   plot(x, y)
   contour(m$xmids, m$ymids, m$mean, add=TRUE, lwd=4, col='pink')
   contour(a$x1out, a$x2out, a$average, add=TRUE, lwd=1)


*/



extern "C" {
    void bin_mean_2d(int *nx, double *x, double *y, double *f,
            int *nxbreaks, double *xbreaks,
            int *nybreaks, double *ybreaks,
            int *number, double *mean)
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
#ifdef DEBUG
        Rprintf("getting space for %d data\n", (*nxbreaks-1)*(*nybreaks-1));
#endif
        for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
            //Rprintf("bij: %d, zero fill\n", bij);
            number[bij] = 0;
            mean[bij] = 0.0;
        }
        for (int i = 0; i < (*nx); i++) {
            if (!ISNA(f[i])) {
                // lower_bound is the the index of the smallest break exceeding x[i];
                // data above the top break yield index nbreak.
                int bi = std::lower_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
                int bj = std::lower_bound(by.begin(), by.end(), y[i]) - by.begin();
                if (bi > 0 && bj > 0 && bi < (*nxbreaks) && bj < (*nybreaks)) {
#ifdef DEBUG
                    Rprintf("x: %6.3f, y: %6.3f, bi: %d, bj: %d\n", x[i], y[i], bi, bj);
#endif
                    number[ij(bi-1, bj-1)]++;
                    mean[ij(bi-1, bj-1)] += f[i];
                }
            }
        }
        for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
            if (number[bij] > 0) {
                mean[bij] = mean[bij] / number[bij];
            } else {
                mean[bij] = NA_REAL;
            }
        }
    }
}

