// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calculate_ranks
NumericVector calculate_ranks(NumericVector f, NumericVector n_out, int n);
RcppExport SEXP _weibulltools_calculate_ranks(SEXP fSEXP, SEXP n_outSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type f(fSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type n_out(n_outSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(calculate_ranks(f, n_out, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_weibulltools_calculate_ranks", (DL_FUNC) &_weibulltools_calculate_ranks, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_weibulltools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}