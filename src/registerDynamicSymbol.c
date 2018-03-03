#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP _oce_trap(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_oce_trap", (DL_FUNC) &_oce_trap, 3},
    {NULL, NULL, 0}
};

void R_init_oce(DllInfo* info) {
    R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

