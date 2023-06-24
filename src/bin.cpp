#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <algorithm>
#include <vector>

//#define DEBUG
//#define DEBUGbc1d
//#define DEBUGbm1d
#define DEBUGba

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
void bin_count_1d(
    int *nx, double *x, int *nxbreaks, double *xbreaks, int *include_lowest,
    int *number)
{
    if (*nxbreaks < 2)
        error("cannot have fewer than 1 break"); // already checked in R but be safe
#ifdef DEBUGbc1d
    Rprintf("bin_count_1d() given *include_lowest=%d\n", *include_lowest);
#endif
    std::vector<double> b(xbreaks, xbreaks + *nxbreaks);
    std::sort(b.begin(), b.end()); // STL wants breaks ordered
    for (int i = 0; i < (*nxbreaks-1); i++) {
        number[i] = 0;
    }
    for (int i = 0; i < (*nx); i++) {
        std::vector<double>::iterator lower;
        lower = std::lower_bound(b.begin(), b.end(), x[i]);
        int bi = lower - b.begin();
        if (bi > 0 && bi < (*nxbreaks)) {
#ifdef DEBUGbc1d
            Rprintf("x: %6.3f   bi: %d    (%f to %f)\n", x[i], bi, xbreaks[bi-1], xbreaks[bi]);
#endif
            number[bi-1]++;
        }
    }
    // optionally, count any x values sitting on the left boundary
    if (*include_lowest != 0) {
#ifdef DEBUGbc1d
        Rprintf("will now see if any x are at lowest break value\n");
#endif
        for (int i = 0; i < (*nx); i++) {
            if (x[i] == xbreaks[0]) {
                number[0]++;
#ifdef DEBUGbc1d
                Rprintf("x=%6.3f included in first bin, yielding number[0]=%d\n", x[i], number[0]);
#endif
            }
        }
    }
}
}

extern "C" {
void bin_mean_1d(int *nx, double *x, double *f, int *nxbreaks, double *xbreaks,
                 int *include_lowest, int *number, double *mean)
{
    if (*nxbreaks < 2)
        error("cannot have fewer than 1 break"); // already checked in R but be safe
#ifdef DEBUGbm1d
    Rprintf("bin_mean_1d() given *include_lowest=%d\n", *include_lowest);
#endif
    std::vector<double> b(xbreaks, xbreaks + *nxbreaks);
    std::sort(b.begin(), b.end()); // STL wants breaks ordered
    for (int i = 0; i < (*nxbreaks-1); i++) {
        number[i] = 0;
        mean[i] = 0.0;
    }
    for (int i = 0; i < (*nx); i++) {
        if (!ISNA(f[i])) {
            std::vector<double>::iterator lower;
            lower = std::lower_bound(b.begin(), b.end(), x[i]);
            int bi = lower - b.begin();
            if (bi > 0 && bi < (*nxbreaks)) {
#ifdef DEBUGbm1d
                Rprintf("  x: %6.3f   bi: %d    (%f to %f)\n", x[i], bi, xbreaks[bi-1], xbreaks[bi]);
#endif
                number[bi-1]++;
                mean[bi-1] += f[i];
            }
        }
    }
    // optionally, incorporate any x values sitting on the left boundary
    if (*include_lowest != 0) {
#ifdef DEBUGbm1d
        Rprintf("  will now see if any x are at lowest break value\n");
#endif
        for (int i = 0; i < (*nx); i++) {
            if (x[i] == xbreaks[0]) {
                number[0]++;
                mean[0] += f[i];
#ifdef DEBUGbm1d
                Rprintf("    x=%6.3f included in first bin\n", x[i]);
#endif
            }
        }
    }
    // finally, divide by the number in each bin, to get mean values
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
            int bi = std::upper_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
            int bj = std::upper_bound(by.begin(), by.end(), y[i]) - by.begin();
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
            int *fill, int *fillgap, int *number, double *mean)
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
                int bi = std::upper_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
                int bj = std::upper_bound(by.begin(), by.end(), y[i]) - by.begin();
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
        if (*fill && *fillgap !=0) { // a logical in R calling functions
#ifdef DEBUG
            int bad = 0;
#endif
            int im, ip, jm, jp;
            // Reminder: ij = j + i * nj, for column-order matrices, so i corresponds to x
            // FIXME: is upper limit in the next loops correct?
            for (int i = 0; i < *nxbreaks-1; i++) {
                for (int j = 0; j < *nybreaks-1; j++) {
                    if (ISNA(mean[ij(i,j)])) {
                        for (im=i-1; im > -1; im--) if (!ISNA(mean[ij(im, j)])) break;
                        for (jm=j-1; jm > -1; jm--) if (!ISNA(mean[ij(i, jm)])) break;
                        // FIXME: is the limit correct on next ... maybe nxbreaks-1 ???
                        for (ip=i+1; ip < *nxbreaks-1; ip++) if (!ISNA(mean[ij(ip, j)])) break;
                        for (jp=j+1; jp < *nybreaks-1; jp++) if (!ISNA(mean[ij(i, jp)])) break;
                        int N=0;
                        double SUM=0.0;
                        if (0 <= im && ip < *(nxbreaks)-1) {
                            if ((*fillgap) < 0 || (*fillgap) >= (ip-im)) {
                                double interpolant = mean[ij(im,j)]+(mean[ij(ip,j)]-mean[ij(im,j)])*(i-im)/(ip-im);
                                SUM += interpolant;
                                N++;
                            }
                        }
                        if (0 <= jm && jp < *(nybreaks)-1) {
                            if ((*fillgap) < 0 || (*fillgap) >= (jp-jm)) {
                                double interpolant = mean[ij(i,jm)]+(mean[ij(i,jp)]-mean[ij(i,jm)])*(j-jm)/(jp-jm);
                                SUM += interpolant;
                                N++;
                            }
                        }
                        if (N > 0) {
                            mean[ij(i, j)] = SUM / N;
                            number[ij(i, j)] = 1; // doesn't have much meaning
                        }
#ifdef DEBUG
                        bad++;
#endif
                    }
                }
            }
#ifdef DEBUG
            Rprintf("nxbreaks: %d, nybreaks: %d\n", *nxbreaks, *nybreaks);
            Rprintf("number of gaps filled: %d\n", bad);
#endif
        }
    }
}
#undef ij

// removed this on 2023-06-24 because it's better for binAverage() to work by
// calling binMean1D().
//
//extern "C" {
//void bin_average(
//    int *nx, double *x, double *y, double *xmin, double *xmax, double *xinc, int *include_lowest,
//    double *means)
//{
//#ifdef DEBUGba
//    Rprintf("*include_lowest = %d\n", *include_lowest);
//#endif
//    if (*nx < 1)
//        error("invalid vector length (%d)", *nx);
//    if (*xmin >= *xmax)
//        error("xmin (%f) may not exceed xmax (%f)", *xmin, *xmax);
//    if (*xinc <= 0)
//        error("cannot have non-positive xinc (%f)", *xinc);
//    // 'b' stands for bin
//    int nb = (int)floor(((*xmax - *xmin) / *xinc));
//#ifdef DEBUGba
//    Rprintf("xmin=%f  xmax=%f  xinc=%f  so calculate nb=%d\n", *xmin, *xmax, *xinc, nb);
//#endif
//    if (nb < 1)
//        error("calculated number of regions (%d) is less than 1", nb);
//    // Get storage using the R allocator, so it can be removed by the R garbage
//    // collector at a later time.
//    int *num = (int*)R_alloc(nb, sizeof(int));
//    for (int b = 0; b < nb; b++) {
//        num[b] = 0;
//        means[b] = 0.0;
//    }
//    int b;
//    for (int i = 0; i < *nx; i++) {
//        // ignore missing data
//        if (ISNA(y[i]))
//            continue;
//        // ignore x values outside the interval from xmin to xmax
//        if (x[i] < *xmin || *xmax < x[i])
//            continue;
//        // find bin to fill (must be in range 0 to nb-1)
//        b = (int)floor((x[i] - *xmin) / (*xinc));
//#ifdef DEBUGba
//        Rprintf("  x[%d]=%f yields b=%d (%.20f) (%.20f)", i, x[i], b, x[i]-*xmin, (x[i]-*xmin)/ *xinc);
//#endif
//        if (b < 0L || b >= nb) {
//            Rprintf("ERROR at x[%d]=%f, computed bin b=%d but it should be from 0 to %d\n",
//                    i, x[i], b, nb);
//            continue;
//        }
//        if (x[i] == *xmin) {
//            if (*include_lowest) {
//                num[b]++;
//                means[b] +=y[i];
//#ifdef DEBUGba
//                Rprintf("  put x[%d]=%f (at xmin), y[%d]=%f into bin b=%d\n", i, x[i], i, y[i], b);
//#endif
//            }
//        } else {
//            num[b]++;
//            means[b] +=y[i];
//#ifdef DEBUGba
//            Rprintf("  put x[%d]=%f, y[%d]=%f into bin b=%d\n", i, x[i], i, y[i], b);
//#endif
//        }
//    }
//    for (int b = 0; b < nb; b++) {
//        if (num[b] > 0)
//            means[b] = means[b] / num[b];
//        else
//            means[b] = NA_REAL;
//    }
//}
//}
