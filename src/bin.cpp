// 2023-06-25 #include <algorithm>
// 2023-06-25 #include <vector>
// 2023-06-25 #include <R.h>
// 2023-06-25 #include <Rdefines.h>
// 2023-06-25 #include <Rinternals.h>
// 2023-06-25
// 2023-06-25 //#define DEBUG
// 2023-06-25 //#define DEBUGbc1d
// 2023-06-25 //#define DEBUGbm1d
// 2023-06-25 #define DEBUGbc2d
// 2023-06-25
// 2023-06-25 extern "C" {
// 2023-06-25 void bin_count_1d(
// 2023-06-25     int *nx, double *x, int *nxbreaks, double *xbreaks, int *include_lowest,
// 2023-06-25     int *number)
// 2023-06-25 {
// 2023-06-25     if (*nxbreaks < 2)
// 2023-06-25         error("cannot have fewer than 1 break"); // already checked in R but be safe
// 2023-06-25 #ifdef DEBUGbc1d
// 2023-06-25     Rprintf("bin_count_1d() given *include_lowest=%d\n", *include_lowest);
// 2023-06-25 #endif
// 2023-06-25     std::vector<double> b(xbreaks, xbreaks + *nxbreaks);
// 2023-06-25     std::sort(b.begin(), b.end()); // STL wants breaks ordered
// 2023-06-25     for (int i = 0; i < (*nxbreaks-1); i++) {
// 2023-06-25         number[i] = 0;
// 2023-06-25     }
// 2023-06-25     for (int i = 0; i < (*nx); i++) {
// 2023-06-25         std::vector<double>::iterator lower;
// 2023-06-25         lower = std::lower_bound(b.begin(), b.end(), x[i]);
// 2023-06-25         int bi = lower - b.begin();
// 2023-06-25         if (0 < bi && bi < (*nxbreaks)) {
// 2023-06-25 #ifdef DEBUGbc1d
// 2023-06-25             Rprintf("x: %6.3f   bi: %d    (%f to %f)\n", x[i], bi, xbreaks[bi-1], xbreaks[bi]);
// 2023-06-25 #endif
// 2023-06-25             number[bi-1]++;
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25     // optionally, count any x values sitting on the left boundary
// 2023-06-25     if (*include_lowest != 0) {
// 2023-06-25 #ifdef DEBUGbc1d
// 2023-06-25         Rprintf("will now see if any x are at lowest break value\n");
// 2023-06-25 #endif
// 2023-06-25         for (int i = 0; i < (*nx); i++) {
// 2023-06-25             if (x[i] == xbreaks[0]) {
// 2023-06-25                 number[0]++;
// 2023-06-25 #ifdef DEBUGbc1d
// 2023-06-25                 Rprintf("x=%6.3f included in first bin, yielding number[0]=%d\n", x[i], number[0]);
// 2023-06-25 #endif
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25 }
// 2023-06-25 }
// 2023-06-25
// 2023-06-25 extern "C" {
// 2023-06-25 void bin_mean_1d(int *nx, double *x, double *f, int *nxbreaks, double *xbreaks,
// 2023-06-25                  int *include_lowest, int *number, double *mean)
// 2023-06-25 {
// 2023-06-25     if (*nxbreaks < 2)
// 2023-06-25         error("cannot have fewer than 1 break"); // already checked in R but be safe
// 2023-06-25 #ifdef DEBUGbm1d
// 2023-06-25     Rprintf("bin_mean_1d() given *include_lowest=%d\n", *include_lowest);
// 2023-06-25 #endif
// 2023-06-25     std::vector<double> b(xbreaks, xbreaks + *nxbreaks);
// 2023-06-25     std::sort(b.begin(), b.end()); // STL wants breaks ordered
// 2023-06-25     for (int i = 0; i < (*nxbreaks-1); i++) {
// 2023-06-25         number[i] = 0;
// 2023-06-25         mean[i] = 0.0;
// 2023-06-25     }
// 2023-06-25     for (int i = 0; i < (*nx); i++) {
// 2023-06-25         if (!ISNA(f[i])) {
// 2023-06-25             std::vector<double>::iterator lower;
// 2023-06-25             lower = std::lower_bound(b.begin(), b.end(), x[i]);
// 2023-06-25             int bi = lower - b.begin();
// 2023-06-25             if (bi > 0 && bi < (*nxbreaks)) {
// 2023-06-25 #ifdef DEBUGbm1d
// 2023-06-25                 Rprintf("  x: %6.3f   bi: %d    (%f to %f)\n", x[i], bi, xbreaks[bi-1], xbreaks[bi]);
// 2023-06-25 #endif
// 2023-06-25                 number[bi-1]++;
// 2023-06-25                 mean[bi-1] += f[i];
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25     // optionally, incorporate any x values sitting on the left boundary
// 2023-06-25     if (*include_lowest != 0) {
// 2023-06-25 #ifdef DEBUGbm1d
// 2023-06-25         Rprintf("  will now see if any x are at lowest break value\n");
// 2023-06-25 #endif
// 2023-06-25         for (int i = 0; i < (*nx); i++) {
// 2023-06-25             if (x[i] == xbreaks[0]) {
// 2023-06-25                 number[0]++;
// 2023-06-25                 mean[0] += f[i];
// 2023-06-25 #ifdef DEBUGbm1d
// 2023-06-25                 Rprintf("    x=%6.3f included in first bin\n", x[i]);
// 2023-06-25 #endif
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25     // finally, divide by the number in each bin, to get mean values
// 2023-06-25     for (int i = 0; i < (*nxbreaks-1); i++) {
// 2023-06-25         if (number[i] > 0) {
// 2023-06-25             mean[i] = mean[i] / number[i];
// 2023-06-25         } else {
// 2023-06-25             mean[i] = NA_REAL;
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25 }
// 2023-06-25 }
// 2023-06-25
// 2023-06-25 // do this in R now
// 2023-06-25 #define ij(i, j) ((i) + (*nxbreaks-1) * (j))
// 2023-06-25 extern "C" {
// 2023-06-25 void bin_count_2d(
// 2023-06-25     int *nx, double *x, double *y,
// 2023-06-25     int *nxbreaks, double *xbreaks,
// 2023-06-25     int *nybreaks, double *ybreaks,
// 2023-06-25     int *include_lowest,
// 2023-06-25     int *number)
// 2023-06-25 {
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25     Rprintf("nxbreaks=%d, nybreaks=%d, include_lowest=%d\n", *nxbreaks, *nybreaks, *include_lowest);
// 2023-06-25 #endif
// 2023-06-25     if (*nxbreaks < 2) error("cannot have fewer than 1 xbreak"); // already checked in R but be safe
// 2023-06-25     if (*nybreaks < 2) error("cannot have fewer than 1 ybreak"); // already checked in R but be safe
// 2023-06-25     std::vector<double> bx(xbreaks, xbreaks + *nxbreaks);
// 2023-06-25     std::sort(bx.begin(), bx.end()); // STL wants breaks ordered
// 2023-06-25     std::vector<double> by(ybreaks, ybreaks + *nybreaks);
// 2023-06-25     std::sort(by.begin(), by.end()); // STL wants breaks ordered
// 2023-06-25     for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
// 2023-06-25         number[bij] = 0;
// 2023-06-25     }
// 2023-06-25     for (int i = 0; i < (*nx); i++) {
// 2023-06-25         int bi = std::lower_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
// 2023-06-25         int bj = std::lower_bound(by.begin(), by.end(), y[i]) - by.begin();
// 2023-06-25         if (0 < bi && 0 < bj && bi < (*nxbreaks) && bj < (*nybreaks)) {
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25             Rprintf("  interior: x=%6.3f, y=%6.3f, bi=%d, bj=%d, ij=%d\n", x[i], y[i], bi, bj, ij(bi-1,bj-1));
// 2023-06-25 #endif
// 2023-06-25             number[ij(bi-1, bj-1)]++;
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25     if (*include_lowest != 0) {
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25         Rprintf("counting points along the left boundary ...\n");
// 2023-06-25 #endif
// 2023-06-25         for (int i = 0; i < (*nx); i++) {
// 2023-06-25             if (x[i] == xbreaks[0]) {
// 2023-06-25                 int bj = std::lower_bound(by.begin(), by.end(), y[i]) - by.begin();
// 2023-06-25                 if (y[i] != ybreaks[0] && 0 < bj && bj < *nybreaks) {
// 2023-06-25                     number[ij(0, bj-1)]++;
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25                     Rprintf("  left edge: x=%6.3f, y=%6.3f, bi=%d, bj=%d, ij=%d\n", x[i], y[i], 0, bj, ij(0,bj-1));
// 2023-06-25 #endif
// 2023-06-25                 }
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25         Rprintf("checking points along the bottom boundary ...\n");
// 2023-06-25 #endif
// 2023-06-25         for (int i = 0; i < (*nx); i++) {
// 2023-06-25             if (y[i] == ybreaks[0]) {
// 2023-06-25                 int bi = std::upper_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
// 2023-06-25                 if (x[i] != xbreaks[0] && 0 < bi && bi < (*nxbreaks)) {
// 2023-06-25                     number[ij(bi-1, 0)]++;
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25                     Rprintf("  bottom edge: x=%6.3f, y=%6.3f, bi=%d, bj=%d, ij=%d\n", x[i], y[i], bi, 0, ij(bi-1,0));
// 2023-06-25 #endif
// 2023-06-25                 }
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25         Rprintf("checking points at bottom-left corner ...\n");
// 2023-06-25 #endif
// 2023-06-25         for (int i = 0; i < (*nx); i++) {
// 2023-06-25             if (x[i] == xbreaks[0] && y[i] == ybreaks[0]) {
// 2023-06-25                 number[ij(0, 0)]++;
// 2023-06-25 #ifdef DEBUGbc2d
// 2023-06-25                 Rprintf("  bottom-left corner: x=%6.3f, y=%6.3f, ij=%d\n", x[i], y[i], ij(0,0));
// 2023-06-25 #endif
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25 }
// 2023-06-25 }
// 2023-06-25 #undef ij
// 2023-06-25 
// 2023-06-25 // do this in R now
// 2023-06-25 #define ij(i, j) ((i) + (*nxbreaks-1) * (j))
// 2023-06-25 extern "C" {
// 2023-06-25     void bin_mean_2d(int *nx, double *x, double *y, double *f,
// 2023-06-25             int *nxbreaks, double *xbreaks,
// 2023-06-25             int *nybreaks, double *ybreaks,
// 2023-06-25             int *fill, int *fillgap, int *number, double *mean)
// 2023-06-25     {
// 2023-06-25 #ifdef DEBUG
// 2023-06-25         Rprintf("nxbreaks: %d, nybreaks: %d\n", *nxbreaks, *nybreaks);
// 2023-06-25 #endif
// 2023-06-25         if (*nxbreaks < 2) error("cannot have fewer than 1 xbreak"); // already checked in R but be safe
// 2023-06-25         if (*nybreaks < 2) error("cannot have fewer than 1 ybreak"); // already checked in R but be safe
// 2023-06-25         std::vector<double> bx(xbreaks, xbreaks + *nxbreaks);
// 2023-06-25         std::sort(bx.begin(), bx.end()); // STL wants breaks ordered
// 2023-06-25         std::vector<double> by(ybreaks, ybreaks + *nybreaks);
// 2023-06-25         std::sort(by.begin(), by.end()); // STL wants breaks ordered
// 2023-06-25         for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
// 2023-06-25             number[bij] = 0;
// 2023-06-25             mean[bij] = 0.0;
// 2023-06-25         }
// 2023-06-25         for (int i = 0; i < (*nx); i++) {
// 2023-06-25             if (!ISNA(f[i])) {
// 2023-06-25                 int bi = std::upper_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
// 2023-06-25                 int bj = std::upper_bound(by.begin(), by.end(), y[i]) - by.begin();
// 2023-06-25                 if (bi > 0 && bj > 0 && bi < (*nxbreaks) && bj < (*nybreaks)) {
// 2023-06-25 #ifdef DEBUG
// 2023-06-25                     Rprintf("x: %6.3f, y: %6.3f, bi: %d, bj: %d\n", x[i], y[i], bi, bj);
// 2023-06-25 #endif
// 2023-06-25                     number[ij(bi-1, bj-1)]++;
// 2023-06-25                     mean[ij(bi-1, bj-1)] += f[i];
// 2023-06-25                 }
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25         for (int bij = 0; bij < (*nxbreaks-1) * (*nybreaks-1); bij++) {
// 2023-06-25             if (number[bij] > 0) {
// 2023-06-25                 mean[bij] = mean[bij] / number[bij];
// 2023-06-25             } else {
// 2023-06-25                 mean[bij] = NA_REAL;
// 2023-06-25             }
// 2023-06-25         }
// 2023-06-25         if (*fill && *fillgap !=0) { // a logical in R calling functions
// 2023-06-25 #ifdef DEBUG
// 2023-06-25             int bad = 0;
// 2023-06-25 #endif
// 2023-06-25             int im, ip, jm, jp;
// 2023-06-25             // Reminder: ij = j + i * nj, for column-order matrices, so i corresponds to x
// 2023-06-25             // FIXME: is upper limit in the next loops correct?
// 2023-06-25             for (int i = 0; i < *nxbreaks-1; i++) {
// 2023-06-25                 for (int j = 0; j < *nybreaks-1; j++) {
// 2023-06-25                     if (ISNA(mean[ij(i,j)])) {
// 2023-06-25                         for (im=i-1; im > -1; im--) if (!ISNA(mean[ij(im, j)])) break;
// 2023-06-25                         for (jm=j-1; jm > -1; jm--) if (!ISNA(mean[ij(i, jm)])) break;
// 2023-06-25                         // FIXME: is the limit correct on next ... maybe nxbreaks-1 ???
// 2023-06-25                         for (ip=i+1; ip < *nxbreaks-1; ip++) if (!ISNA(mean[ij(ip, j)])) break;
// 2023-06-25                         for (jp=j+1; jp < *nybreaks-1; jp++) if (!ISNA(mean[ij(i, jp)])) break;
// 2023-06-25                         int N=0;
// 2023-06-25                         double SUM=0.0;
// 2023-06-25                         if (0 <= im && ip < *(nxbreaks)-1) {
// 2023-06-25                             if ((*fillgap) < 0 || (*fillgap) >= (ip-im)) {
// 2023-06-25                                 double interpolant = mean[ij(im,j)]+(mean[ij(ip,j)]-mean[ij(im,j)])*(i-im)/(ip-im);
// 2023-06-25                                 SUM += interpolant;
// 2023-06-25                                 N++;
// 2023-06-25                             }
// 2023-06-25                         }
// 2023-06-25                         if (0 <= jm && jp < *(nybreaks)-1) {
// 2023-06-25                             if ((*fillgap) < 0 || (*fillgap) >= (jp-jm)) {
// 2023-06-25                                 double interpolant = mean[ij(i,jm)]+(mean[ij(i,jp)]-mean[ij(i,jm)])*(j-jm)/(jp-jm);
// 2023-06-25                                 SUM += interpolant;
// 2023-06-25                                 N++;
// 2023-06-25                             }
// 2023-06-25                         }
// 2023-06-25                         if (N > 0) {
// 2023-06-25                             mean[ij(i, j)] = SUM / N;
// 2023-06-25                             number[ij(i, j)] = 1; // doesn't have much meaning
// 2023-06-25                         }
// 2023-06-25 #ifdef DEBUG
// 2023-06-25                         bad++;
// 2023-06-25 #endif
// 2023-06-25                     }
// 2023-06-25                 }
// 2023-06-25             }
// 2023-06-25 #ifdef DEBUG
// 2023-06-25             Rprintf("nxbreaks: %d, nybreaks: %d\n", *nxbreaks, *nybreaks);
// 2023-06-25             Rprintf("number of gaps filled: %d\n", bad);
// 2023-06-25 #endif
// 2023-06-25         }
// 2023-06-25     }
// 2023-06-25 }
// 2023-06-25 #undef ij

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
