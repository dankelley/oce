// 2014 oct 2 having troubles with Makevars; maybe consult
// https://github.com/cran/PopGenome/blob/master/src/whopgenome/whopgen_filtering.cpp
// to see how they do it.  (Try building that first.)

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <algorithm>
#include <vector>

//#define DEBUG

// These functions use the STL function lower_bound to find the index of the
// smallest break exceeding x[i].  Data exceeding the top break get index equal
// to nbreak.


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
    void bin_count_1d(int *nx, double *x, int *nxbreaks, double *xbreaks,
            int *number, double *mean)
    {

        if (*nxbreaks < 2)
            error("cannot have fewer than 1 break"); // already checked in R but be safe
        std::vector<double> b(xbreaks, xbreaks + *nxbreaks);
        std::sort(b.begin(), b.end()); // STL wants breaks ordered
        for (int i = 0; i < (*nxbreaks-1); i++) {
            number[i] = 0;
        }
        for (int i = 0; i < (*nx); i++) {
            std::vector<double>::iterator lower_bound;
            lower_bound = std::lower_bound(b.begin(), b.end(), x[i]);
            int bi = lower_bound - b.begin();
            if (bi > 0 && bi < (*nxbreaks)) {
#ifdef DEBUG
                Rprintf("x: %6.3f   bi: %d    (%f to %f)\n", x[i], bi, xbreaks[bi-1], xbreaks[bi]);
#endif
                number[bi-1]++;
            }
        }
    }
}

extern "C" {
    void bin_mean_1d(int *nx, double *x, double *f, int *nxbreaks, double *xbreaks,
            int *number, double *mean)
    {

        if (*nxbreaks < 2)
            error("cannot have fewer than 1 break"); // already checked in R but be safe
        std::vector<double> b(xbreaks, xbreaks + *nxbreaks);
        std::sort(b.begin(), b.end()); // STL wants breaks ordered
        for (int i = 0; i < (*nxbreaks-1); i++) {
            number[i] = 0;
            mean[i] = 0.0;
        }
        for (int i = 0; i < (*nx); i++) {
            if (!ISNA(f[i])) {
                std::vector<double>::iterator lower_bound;
                lower_bound = std::lower_bound(b.begin(), b.end(), x[i]);
                int bi = lower_bound - b.begin();
                if (bi > 0 && bi < (*nxbreaks)) {
#ifdef DEBUG
                    Rprintf("x: %6.3f   bi: %d    (%f to %f)\n", x[i], bi, xbreaks[bi-1], xbreaks[bi]);
#endif
                    number[bi-1]++;
                    mean[bi-1] += f[i];
                }
            }
        }
        for (int i = 0; i < (*nxbreaks-1); i++) {
            if (number[i] > 0) {
                mean[i] = mean[i] / number[i];
            } else {
                mean[i] = NA_REAL;
            }
        }
    }
}


#define ij(i, j) ((i) + (*nxbreaks-1) * (j))
extern "C" {
    void bin_count_2d(int *nx, double *x, double *y,
            int *nxbreaks, double *xbreaks,
            int *nybreaks, double *ybreaks,
            int *number, double *mean)
    {
#ifdef DEBUG
        Rprintf("nxbreaks: %d, nybreaks: %d\n", *nxbreaks, *nybreaks);
#endif
        if (*nxbreaks < 2) error("cannot have fewer than 1 xbreak"); // already checked in R but be safe
        if (*nybreaks < 2) error("cannot have fewer than 1 ybreak"); // already checked in R but be safe
        std::vector<double> bx(xbreaks, xbreaks + *nxbreaks);
        std::sort(bx.begin(), bx.end()); // STL wants breaks ordered
        std::vector<double> by(ybreaks, ybreaks + *nybreaks);
        std::sort(by.begin(), by.end()); // STL wants breaks ordered
        for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
            number[bij] = 0;
        }
        for (int i = 0; i < (*nx); i++) {
            int bi = std::lower_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
            int bj = std::lower_bound(by.begin(), by.end(), y[i]) - by.begin();
            if (bi > 0 && bj > 0 && bi < (*nxbreaks) && bj < (*nybreaks)) {
#ifdef DEBUG
                Rprintf("x: %6.3f, y: %6.3f, bi: %d, bj: %d\n", x[i], y[i], bi, bj);
#endif
                number[ij(bi-1, bj-1)]++;
            }
        }
    }
}
#undef ij


#define ij(i, j) ((i) + (*nxbreaks-1) * (j))
extern "C" {
    void bin_mean_2d(int *nx, double *x, double *y, double *f,
            int *nxbreaks, double *xbreaks,
            int *nybreaks, double *ybreaks,
            int *fill, int *number, double *mean)
    {
#ifdef DEBUG
        Rprintf("nxbreaks: %d, nybreaks: %d\n", *nxbreaks, *nybreaks);
#endif
        if (*nxbreaks < 2) error("cannot have fewer than 1 xbreak"); // already checked in R but be safe
        if (*nybreaks < 2) error("cannot have fewer than 1 ybreak"); // already checked in R but be safe
        std::vector<double> bx(xbreaks, xbreaks + *nxbreaks);
        std::sort(bx.begin(), bx.end()); // STL wants breaks ordered
        std::vector<double> by(ybreaks, ybreaks + *nybreaks);
        std::sort(by.begin(), by.end()); // STL wants breaks ordered
        for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
            number[bij] = 0;
            mean[bij] = 0.0;
        }
        for (int i = 0; i < (*nx); i++) {
            if (!ISNA(f[i])) {
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
        if (*fill) {
            int bad = 0;
            for (int i = 0; i < (*nxbreaks-1); i++) {
                for (int j = 0; j < (*nybreaks-1); j++) {
                    if (ISNA(mean[ij(i,j)])) {
                        int N=0;
                        double MEAN=0.0;
                        int i0 = i-1;
                        // fix across i
                        for (; i0 > -1; i0--) {
                            if (!ISNA(mean[ij(i0, j)])) {
                                break;
                            }
                        }
                        int i1 = i+1;
                        for (; i1 < (*nxbreaks-1); i1++) {
                            if (!ISNA(mean[ij(i1, j)])) {
                                break;
                            }
                        }
                        int j0 = j-1;
                        // fix across i
                        for (; j0 > -1; j0--) {
                            if (!ISNA(mean[ij(i, j0)])) {
                                break;
                            }
                        }
                        int j1 = j+1;
                        for (; j1 < (*nybreaks-1); j1++) {
                            if (!ISNA(mean[ij(i, j1)])) {
                                break;
                            }
                        }
                        if (bad < 100) {
                            Rprintf("i:%d, j:%d, i0:%d, i1:%d, j0:%d, j1:%d, nxbreaks:%d\n", i,j,i0,i1,j0,j1,(*nxbreaks));
                        }
                        bad++;
                    }
                }
            }
            Rprintf("number of gaps to fill: %d\n", bad);
        }
    }
}
#undef ij

