#include <Rcpp.h>
#include <cmath>
#include <string>
#include <math.h>
#include <numeric>
#include <chrono>
#include "netinf_utilities.h"

using namespace Rcpp;



int get_index(IntegerVector x, int val) {
    for(int i = 0; i < x.size(); i++) {
        if(x[i] == val) return i;
    }
    return -1;
}

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
    if (estimate > 1000 & estimate < 60000) {
        estimate /= 1000;  
        unit = "seconds";
    } else if (estimate > 60000 & estimate < 3600000) {
        estimate /= 60000;
        unit = "minutes";
    } else if (estimate > 3600000 & estimate < 86400000) {
        estimate /= 3600000;
        unit = "hours";
    } else if (estimate > 86400000) {
        estimate /= 86400000;
        unit = "days";
    }
    float out = roundf(estimate * 100) / 100;
    Rcout << message << out << " " << unit << ".\n";
}

time_point print_timing(time_point start_time, std::string step) {
    time_point t2 = Clock::now();
    time_duration fp_ms = t2 - start_time;
    Rcout << step << "took: " << fp_ms.count() << "ms\n";
    return t2;
}