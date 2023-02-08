// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// getMax
int getMax(IntegerVector arr, int n);
RcppExport SEXP _TriRadix_getMax(SEXP arrSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(getMax(arr, n));
    return rcpp_result_gen;
END_RCPP
}
// countSort
void countSort(IntegerVector arr, int n, int exp);
RcppExport SEXP _TriRadix_countSort(SEXP arrSEXP, SEXP nSEXP, SEXP expSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type exp(expSEXP);
    countSort(arr, n, exp);
    return R_NilValue;
END_RCPP
}
// radixsort
IntegerVector radixsort(IntegerVector arr);
RcppExport SEXP _TriRadix_radixsort(SEXP arrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type arr(arrSEXP);
    rcpp_result_gen = Rcpp::wrap(radixsort(arr));
    return rcpp_result_gen;
END_RCPP
}
// get_digit
int get_digit(int num, int d);
RcppExport SEXP _TriRadix_get_digit(SEXP numSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type num(numSEXP);
    Rcpp::traits::input_parameter< int >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(get_digit(num, d));
    return rcpp_result_gen;
END_RCPP
}
// counting_sort_by_digit
NumericVector counting_sort_by_digit(NumericVector arr, int digit_position);
RcppExport SEXP _TriRadix_counting_sort_by_digit(SEXP arrSEXP, SEXP digit_positionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< int >::type digit_position(digit_positionSEXP);
    rcpp_result_gen = Rcpp::wrap(counting_sort_by_digit(arr, digit_position));
    return rcpp_result_gen;
END_RCPP
}
// radix_sort_rcpp
NumericVector radix_sort_rcpp(NumericVector arr);
RcppExport SEXP _TriRadix_radix_sort_rcpp(SEXP arrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr(arrSEXP);
    rcpp_result_gen = Rcpp::wrap(radix_sort_rcpp(arr));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_TriRadix_getMax", (DL_FUNC) &_TriRadix_getMax, 2},
    {"_TriRadix_countSort", (DL_FUNC) &_TriRadix_countSort, 3},
    {"_TriRadix_radixsort", (DL_FUNC) &_TriRadix_radixsort, 1},
    {"_TriRadix_get_digit", (DL_FUNC) &_TriRadix_get_digit, 2},
    {"_TriRadix_counting_sort_by_digit", (DL_FUNC) &_TriRadix_counting_sort_by_digit, 2},
    {"_TriRadix_radix_sort_rcpp", (DL_FUNC) &_TriRadix_radix_sort_rcpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_TriRadix(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
