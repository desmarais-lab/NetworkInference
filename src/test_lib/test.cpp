#include <Rcpp.h>
//#include "test_support_functions.h"
using namespace Rcpp;


NumericVector external_test_function(NumericVector x) {
  return x * 2;
}
