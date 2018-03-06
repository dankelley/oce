#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


extern SEXP _oce_do_approx3d(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_fill_gap_1d(SEXP, SEXP);
extern SEXP _oce_do_geoddist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP f);
extern SEXP _oce_do_geoddist_alongpath(SEXP, SEXP, SEXP, SEXP f);
extern SEXP _oce_do_geod_xy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_geod_xy_inverse(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_oce_convolve(SEXP, SEXP, SEXP);
extern SEXP _oce_do_oce_filter(SEXP, SEXP, SEXP);
extern SEXP _oce_do_matrix_smooth(SEXP);
extern SEXP _oce_do_runlm(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_trap(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_oce_do_approx3d", (DL_FUNC) &_oce_do_approx3d, 7},
    {"_oce_do_fill_gap_1d", (DL_FUNC) &_oce_do_fill_gap_1d, 2},
    {"_oce_do_geoddist", (DL_FUNC) &_oce_do_geoddist, 6},
    {"_oce_do_geod_xy", (DL_FUNC) &_oce_do_geod_xy, 6},
    {"_oce_do_geod_xy_inverse", (DL_FUNC) &_oce_do_geod_xy_inverse, 6},
    {"_oce_do_geoddist_alongpath", (DL_FUNC) &_oce_do_geoddist_alongpath, 4},
    {"_oce_do_oce_filter", (DL_FUNC) &_oce_do_oce_filter, 3},
    {"_oce_do_oce_convolve", (DL_FUNC) &_oce_do_oce_convolve, 3},
    {"_oce_do_matrix_smooth", (DL_FUNC) &_oce_do_matrix_smooth, 1},
    {"_oce_do_runlm", (DL_FUNC) &_oce_do_runlm, 5},
    {"_oce_do_trap", (DL_FUNC) &_oce_do_trap, 3},
    {NULL, NULL, 0}
};

void R_init_oce(DllInfo* info) {
    R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

