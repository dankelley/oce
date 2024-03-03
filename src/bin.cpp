#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <algorithm>
#include <vector>

// uncomment next to debug bin_count_2d
// #define DEBUG_COUNT

// 1D code: in R now; see
// https://github.com/dankelley/oce/tree/37d0499803e6bbb30aa93c083ea07ef22b434dd8
// for the old code, temporarily below, but commented-out,
// whilst working on issue 2199.

// 2D code

#define ij(i, j) ((i) + (*nxbreaks - 1) * (j))
extern "C" {
void bin_count_2d(int *nx, double *x, double *y, int *nxbreaks, double *xbreaks,
                  int *nybreaks, double *ybreaks, int *include_lowest,
                  int *number) {
#ifdef DEBUG_COUNT
  Rprintf("nxbreaks=%d, nybreaks=%d, include_lowest=%d\n", *nxbreaks, *nybreaks,
          *include_lowest);
#endif
  if (*nxbreaks < 2)
    error(
        "cannot have fewer than 1 xbreak"); // already checked in R but be safe
  if (*nybreaks < 2)
    error(
        "cannot have fewer than 1 ybreak"); // already checked in R but be safe
  std::vector<double> bx(xbreaks, xbreaks + *nxbreaks);
  std::sort(bx.begin(), bx.end()); // STL wants breaks ordered
  std::vector<double> by(ybreaks, ybreaks + *nybreaks);
  std::sort(by.begin(), by.end()); // STL wants breaks ordered
  for (int bij = 0; bij < (*nxbreaks - 1) * (*nybreaks - 1); bij++) {
    number[bij] = 0;
  }
  for (int i = 0; i < (*nx); i++) {
    int bi = std::lower_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
    int bj = std::lower_bound(by.begin(), by.end(), y[i]) - by.begin();
    if (0 < bi && 0 < bj && bi < (*nxbreaks) && bj < (*nybreaks)) {
#ifdef DEBUG_COUNT
      Rprintf("  interior: x=%6.3f, y=%6.3f, bi=%d, bj=%d, ij=%d\n", x[i], y[i],
              bi, bj, ij(bi - 1, bj - 1));
#endif
      number[ij(bi - 1, bj - 1)]++;
    }
  }
  if (*include_lowest != 0) {
#ifdef DEBUG_COUNT
    Rprintf("counting points along the left boundary ...\n");
#endif
    for (int i = 0; i < (*nx); i++) {
      if (x[i] == xbreaks[0]) {
        int bj = std::lower_bound(by.begin(), by.end(), y[i]) - by.begin();
        if (y[i] != ybreaks[0] && 0 < bj && bj < *nybreaks) {
          number[ij(0, bj - 1)]++;
#ifdef DEBUG_COUNT
          Rprintf("  left edge: x=%6.3f, y=%6.3f, bi=%d, bj=%d, ij=%d\n", x[i],
                  y[i], 0, bj, ij(0, bj - 1));
#endif
        }
      }
    }
#ifdef DEBUG_COUNT
    Rprintf("checking points along the bottom boundary ...\n");
#endif
    for (int i = 0; i < (*nx); i++) {
      if (y[i] == ybreaks[0]) {
        int bi = std::upper_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
        if (x[i] != xbreaks[0] && 0 < bi && bi < (*nxbreaks)) {
          number[ij(bi - 1, 0)]++;
#ifdef DEBUG_COUNT
          Rprintf("  bottom edge: x=%6.3f, y=%6.3f, bi=%d, bj=%d, ij=%d\n",
                  x[i], y[i], bi, 0, ij(bi - 1, 0));
#endif
        }
      }
    }
#ifdef DEBUG_COUNT
    Rprintf("checking points at bottom-left corner ...\n");
#endif
    for (int i = 0; i < (*nx); i++) {
      if (x[i] == xbreaks[0] && y[i] == ybreaks[0]) {
        number[ij(0, 0)]++;
#ifdef DEBUG_COUNT
        Rprintf("  bottom-left corner: x=%6.3f, y=%6.3f, ij=%d\n", x[i], y[i],
                ij(0, 0));
#endif
      }
    }
  }
}
}
#undef ij

#define ij(i, j) ((i) + (*nxbreaks - 1) * (j))
extern "C" {
void bin_mean_2d(int *nx, double *x, double *y, double *f, int *nxbreaks,
                 double *xbreaks, int *nybreaks, double *ybreaks, int *fill,
                 int *fillgap, int *number, double *mean, int *debug) {
  if (*debug > 0) {
    Rprintf("in bin_mean_2d() with nx=%d nxbreaks=%d nybreaks=%d fill=%d "
            "fillgap=%d\n",
            *nx, *nxbreaks, *nybreaks, *fill, *fillgap);
  }
  if (*nxbreaks < 2)
    error(
        "cannot have fewer than 1 xbreak"); // already checked in R but be safe
  if (*nybreaks < 2)
    error(
        "cannot have fewer than 1 ybreak"); // already checked in R but be safe
  std::vector<double> bx(xbreaks, xbreaks + *nxbreaks);
  std::sort(bx.begin(), bx.end()); // STL wants breaks ordered
  std::vector<double> by(ybreaks, ybreaks + *nybreaks);
  std::sort(by.begin(), by.end()); // STL wants breaks ordered
  for (int bij = 0; bij < (*nxbreaks - 1) * (*nybreaks - 1); bij++) {
    number[bij] = 0;
    mean[bij] = 0.0;
  }
  for (int i = 0; i < (*nx); i++) {
    if (!ISNA(f[i])) {
      int bi = std::upper_bound(bx.begin(), bx.end(), x[i]) - bx.begin();
      int bj = std::upper_bound(by.begin(), by.end(), y[i]) - by.begin();
      if (bi > 0 && bj > 0 && bi < (*nxbreaks) && bj < (*nybreaks)) {
        if (*debug > 1) {
          Rprintf("x: %6.3f, y: %6.3f, bi: %d, bj: %d\n", x[i], y[i], bi, bj);
        }
        number[ij(bi - 1, bj - 1)]++;
        mean[ij(bi - 1, bj - 1)] += f[i];
      }
    }
  }
  for (int bij = 0; bij < (*nxbreaks - 1) * (*nybreaks - 1); bij++) {
    if (number[bij] > 0) {
      mean[bij] = mean[bij] / number[bij];
    } else {
      mean[bij] = NA_REAL;
    }
  }
  // fill gaps, if appropriate
  if (*fill && *fillgap != 0) { // a logical in R calling functions
    int bad = 0;
    int im, ip, jm, jp;
    // Reminder: ij = j + i * nj, for column-order matrices, so i corresponds to
    // x
    // FIXME: is upper limit in the next loops correct?
    for (int i = 0; i < *nxbreaks - 1; i++) {
      for (int j = 0; j < *nybreaks - 1; j++) {
        if (ISNA(mean[ij(i, j)])) {
          // find im,ip (indices of good neighbours in i), and similarly jm,jp
          for (im = i - 1; im > -1; im--)
            if (!ISNA(mean[ij(im, j)]))
              break;
          for (jm = j - 1; jm > -1; jm--)
            if (!ISNA(mean[ij(i, jm)]))
              break;
          // FIXME: is the limit correct on next ... maybe nxbreaks-1 ???
          for (ip = i + 1; ip < *nxbreaks - 1; ip++)
            if (!ISNA(mean[ij(ip, j)]))
              break;
          for (jp = j + 1; jp < *nybreaks - 1; jp++)
            if (!ISNA(mean[ij(i, jp)]))
              break;
          int N = 0;
          double SUM = 0.0;
          // can only fill if good neighbours exist (not at edges)
          if (0 <= im && ip < *(nxbreaks)-1) {
            if ((*fillgap) < 0 || (*fillgap) >= (ip - im)) {
              if (*debug > 0) {
                Rprintf("mean[%d,%d]=NA but mean[c(%d,%d),]=(%.4g,%.4g)\n", i,
                        j, im, ip, mean[ij(im, j)], mean[ij(ip, j)]);
              }
              double interpolant =
                  mean[ij(im, j)] +
                  (mean[ij(ip, j)] - mean[ij(im, j)]) * (i - im) / (ip - im);
              SUM += interpolant;
              N++;
            }
          }
          if (0 <= jm && jp < *(nybreaks)-1) {
            if ((*fillgap) < 0 || (*fillgap) >= (jp - jm)) {
              if (*debug > 0) {
                Rprintf("mean[%d,%d]=NA but mean[,c(%d,%d)]=(%.4g,%.4g)\n", i,
                        j, jm, jp, mean[ij(i, jm)], mean[ij(i, jp)]);
              }
              double interpolant =
                  mean[ij(i, jm)] +
                  (mean[ij(i, jp)] - mean[ij(i, jm)]) * (j - jm) / (jp - jm);
              SUM += interpolant;
              N++;
            }
          }
          if (N > 0) {
            mean[ij(i, j)] = SUM / N;
            number[ij(i, j)] = 1; // doesn't have much meaning
            if (*debug > 0) {
              Rprintf("    set mean[%d,%d] to %.4g, using interpolation in %d direction(s)\n", i, j,
                      mean[ij(i, j)], N);
            }
            bad++;
          }
        }
      }
    }
    if (*debug > 0) {
      Rprintf("bin_mean_2d() encountered (and filled) %d internal gaps\n", bad);
    }
  }
}
}
#undef ij
