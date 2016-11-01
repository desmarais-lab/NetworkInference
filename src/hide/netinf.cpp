#include <Rcpp.h>
#include "stdafx.h"
#include "cascnetinf.h"

using namespace Rcpp;

// [[Rcpp::export]]
int test_netinf() {
  TNetInfBs NIB(false, false);
  return 10;
}
