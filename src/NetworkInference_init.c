#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP NetworkInference_count_possible_edges_(SEXP, SEXP);
extern SEXP NetworkInference_netinf_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"NetworkInference_count_possible_edges_", (DL_FUNC) &NetworkInference_count_possible_edges_, 2},
    {"NetworkInference_netinf_",               (DL_FUNC) &NetworkInference_netinf_,               6},
    {NULL, NULL, 0}
};

void R_init_NetworkInference(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}