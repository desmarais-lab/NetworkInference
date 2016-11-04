#include <Rcpp.h>
#include "Snap.h"
#include "cascnetinf.h"
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
    return x * 2;
}

// Run netinf on file
// [[Rcpp::export]]
void test_netinf() {
    
    // Default settings
    const char* infile_name = "data/example-cascades.txt";
    int model = 0;
    double alpha = 1.0;
    int iters = 5;
    
    // Initialize netinf obj
    TNetInfBs NIB(true, false);
    
    // Load cascades from file
    //TFIn FIn("/Users/flinder/Dropbox/current_projects/netinf/data/example-cascades.txt");
    TFIn FIn(infile_name);
    
    NIB.LoadCascadesTxt(FIn, model, alpha);
    
    // Run netinf 
    NIB.Init();
    //printf("cascades:%d nodes:%d potential edges:%d\nRunning NETINF...\n", 
    //       NIB.GetCascs(), NIB.GetNodes(), NIB.CascPerEdge.Len());
    NIB.GreedyOpt(iters);
    //
    ////Save network
    TStr outfile_name();
    NIB.SavePlaneTextNet("data/test_out.txt");
}