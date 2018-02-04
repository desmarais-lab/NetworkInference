#include <Rcpp.h>
#include <cmath>
#include <string>
#include <math.h>
#include <numeric>
#include "netinf_utilities.h"

using namespace Rcpp;

double dexp_(float x, float lambda) {
    return lambda * std::exp(-1 * lambda * x);
}

// Rayleigh density
double drayleigh_(float x, float lambda) {
    return (x / pow(lambda, 2)) * std::exp(-pow(x, 2) / (2 * pow(lambda, 2)));
}

// Normal CDF from: https://www.johndcook.com/blog/cpp_phi/
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

// Get index of value (first one that matches) in Rcpp Integer Vector
int which_int_(int value, IntegerVector x) {
    int n = x.size();
    for(int i = 0; i < n; i++) {
        if(x[i] == value) {
            return i;
        }
    }
    return -1; 
}

// Union of two integer vectors with unique elements
void update_children_(IntegerVector &children, IntegerVector &candidates) {
    int nc = candidates.size();
    for(int i = 0; i < nc; i++) {
        int k = which_int_(candidates[i], children);
        if(k == -1) {
            children.push_back(candidates[i]);
        }
    }
}

// Creates a string pair id from two integer node ids
std::string make_pair_id_(int &u, int &v) {
    return std::to_string(u) + "_" + std::to_string(v);
}

// Sum up rcpp vector excluding nan values (roots of the trees, i.e. nodes 
// w/o parents)
double sum_vector(NumericVector x) {
    double out = 0;
    for(int i = 0; i < x.size(); i++)  {
        if(std::isnan(x[i])) continue;
        out += x[i];
    }
    return out;
}

NumericVector copy_vector(NumericVector x) {
    NumericVector out(x.size());
    for(int i = 0; i < x.size(); i++) out[i] = x[i];
    return out;
}

void print_time_estimate(std::chrono::duration<double, std::milli> fp_ms,
                         bool auto_edges, int n_edges) {
    float estimate;
    std::string message;
    if(auto_edges) {
        estimate = fp_ms.count();
        message = "Estimated time per edge: ";
    } else {
        estimate = fp_ms.count() * n_edges;
        message = "Estimated completion time: ";
    }
    std::string unit = "milliseconds";
    if (estimate > 1000) {
        estimate /= 1000;  
        unit = "seconds";
    } else if (estimate > 60000) {
        estimate /= 60000;
        unit = "minutes";
    } else if (estimate > 3600000) {
        estimate /= 3600000;
        unit = "hours";
    } else if (estimate > 86400000) {
        estimate /= 86400000;
        unit = "days";
    }
    float out = roundf(estimate * 100) / 100;
    Rcout << message << out << " " << unit << ".\n";
}

