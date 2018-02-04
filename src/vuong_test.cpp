#include <Rcpp.h>
#include "netinf_utilities.h"

using namespace Rcpp;

double vuong_test(NumericVector x1, NumericVector x2, bool bic=false) {
    //x1: old (k edges), x2: new (k+1 edges)
    //x2 = x2 - (log(float(x2.size())) / (2 * float(x2.size())));
    x2 = x2 - (1 / float(x2.size()));
    NumericVector liks = x2 - x1;
    double sd = Rcpp::sd(liks);
    double stat = sum(liks) / (sd * sqrt(float(x2.size())));
    double pval = 1 - normal_cdf(stat);
    return pval;
}