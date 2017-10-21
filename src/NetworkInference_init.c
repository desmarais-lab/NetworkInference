#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _NetworkInference_count_possible_edges_(SEXP, SEXP);
extern SEXP _NetworkInference_netinf_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_NetworkInference_count_possible_edges_", (DL_FUNC) &_NetworkInference_count_possible_edges_, 2},
    {"_NetworkInference_netinf_",               (DL_FUNC) &_NetworkInference_netinf_,               6},
    {NULL, NULL, 0}
};

void R_init_NetworkInference(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}