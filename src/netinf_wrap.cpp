#include <Rcpp.h>
#include "test_lib/test.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector wrapper_(NumericVector x) {
    return external_test_function(x);
}
