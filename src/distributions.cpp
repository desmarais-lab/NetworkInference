#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

double dexp_(double x, double lambda) {
    return lambda * std::exp(-1 * lambda * x);
}

double drayleigh_(double x, double lambda) {
    return (x / pow(lambda, 2)) * std::exp(-pow(x, 2) / (2 * pow(lambda, 2)));
}

double dlognormal_(double x, double mu, double sigma) {
    if(x <= 0) {
        std::string msg = "x outside support of log-normal distribution.\n";
        throw std::invalid_argument(msg);
    }
    return 1 / (x*sigma*sqrt(2*M_PI)) * 
        std::exp(-(pow((log(x) - mu), 2)/(2*pow(sigma, 2))));
}

// https://www.johndcook.com/blog/cpp_phi/
double normal_cdf(double x) {
    // constants
    double a1 =  0.254829592;
    double a2 = -0.284496736;
    double a3 =  1.421413741;
    double a4 = -1.453152027;
    double a5 =  1.061405429;
    double p  =  0.3275911;
    
    // Save the sign of x
    int sign = 1;
    if (x < 0)
    sign = -1;
    x = fabs(x)/sqrt(2.0);
    
    // A&S formula 7.1.26
    double t = 1.0/(1.0 + p*x);
    double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);
    
    return 0.5*(1.0 + sign*y);
}
