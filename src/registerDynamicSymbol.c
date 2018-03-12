#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>



extern SEXP _oce_bilinearInterp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_adv_vector_time(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_amsr_composite(SEXP, SEXP);
extern SEXP _oce_do_amsr_average(SEXP, SEXP);
extern SEXP _oce_do_approx3d(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_curl1(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_curl2(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_fill_gap_1d(SEXP, SEXP);
extern SEXP _oce_do_geoddist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP f);
extern SEXP _oce_do_geoddist_alongpath(SEXP, SEXP, SEXP, SEXP f);
extern SEXP _oce_do_geod_xy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_geod_xy_inverse(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_get_bit(SEXP, int);
extern SEXP _oce_do_gradient(SEXP, SEXP, SEXP);
extern SEXP _oce_do_interp_barnes(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_ldc_rdi_in_file(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_ldc_sontek_adp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_oceApprox(SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_oce_convolve(SEXP, SEXP, SEXP);
extern SEXP _oce_do_oce_filter(SEXP, SEXP, SEXP);
extern SEXP _oce_do_matrix_smooth(SEXP);
extern SEXP _oce_do_runlm(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_sfm_enu(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _oce_do_trap(SEXP, SEXP, SEXP);
extern SEXP _oce_trim_ts(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_oce_bilinearInterp", (DL_FUNC) &_oce_bilinearInterp, 5},
    {"_oce_do_adv_vector_time", (DL_FUNC) &_oce_do_adv_vector_time, 7},
    {"_oce_do_amsr_average", (DL_FUNC) &_oce_do_amsr_average, 2},
    {"_oce_do_amsr_composite", (DL_FUNC) &_oce_do_amsr_composite, 2},
    {"_oce_do_approx3d", (DL_FUNC) &_oce_do_approx3d, 7},
    {"_oce_do_curl1", (DL_FUNC) &_oce_do_curl1, 5},
    {"_oce_do_curl2", (DL_FUNC) &_oce_do_curl2, 5},
    {"_oce_do_fill_gap_1d", (DL_FUNC) &_oce_do_fill_gap_1d, 2},
    {"_oce_do_geoddist", (DL_FUNC) &_oce_do_geoddist, 6},
    {"_oce_do_interp_barnes", (DL_FUNC) &_oce_do_interp_barnes, 10},
    {"_oce_do_geod_xy", (DL_FUNC) &_oce_do_geod_xy, 6},
    {"_oce_do_geod_xy_inverse", (DL_FUNC) &_oce_do_geod_xy_inverse, 6},
    {"_oce_do_geoddist_alongpath", (DL_FUNC) &_oce_do_geoddist_alongpath, 4},
    {"_oce_do_get_bit", (DL_FUNC) &_oce_do_get_bit, 2},
    {"_oce_do_gradient", (DL_FUNC) &_oce_do_gradient, 3},
    {"_oce_do_ldc_rdi_in_file", (DL_FUNC) &_oce_do_ldc_rdi_in_file, 5},
    {"_oce_do_ldc_sontek_adp", (DL_FUNC) &_oce_do_ldc_sontek_adp, 6},
    {"_oce_do_oceApprox", (DL_FUNC) &_oce_do_oceApprox, 4},
    {"_oce_do_oce_filter", (DL_FUNC) &_oce_do_oce_filter, 3},
    {"_oce_do_oce_convolve", (DL_FUNC) &_oce_do_oce_convolve, 3},
    {"_oce_do_matrix_smooth", (DL_FUNC) &_oce_do_matrix_smooth, 1},
    {"_oce_do_runlm", (DL_FUNC) &_oce_do_runlm, 5},
    {"_oce_do_sfm_enu", (DL_FUNC) &_oce_do_sfm_enu, 6},
    {"_oce_do_trap", (DL_FUNC) &_oce_do_trap, 3},
    {"_oce_trim_ts", (DL_FUNC) &_oce_trim_ts, 3},
    {NULL, NULL, 0}
};

void R_init_oce(DllInfo* info) {
    R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

