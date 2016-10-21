#include <Rcpp.h>
using namespace Rcpp;

NumericVector support_function(NumericVector x) {
  return x * 2;
}
