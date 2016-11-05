#define SIZE_MAX 18446744073709551615UL
#include <Rcpp.h>
#include "Snap.h"
#include "cascnetinf.h"
using namespace Rcpp;



//' Run the netinf algorithm on a set of nodes and cascades
//' 
//' @param node_ids An integer vector of integer node ids.
//' @param node_names A charcter vector of node names.
//' @param cascade_ids A list of integer vectors containing the node ids of
//'     the cascade in order of infection.
//' @param  cascade_times A list of numeric vectors each containing infection 
//'     times for the correspoinding nodes in \code{cascade_ids}.
//' @param model integer indicating the choice of model: 0: exponential, 
//'     1: power law, 2: rayleigh.
//' @param verbose boolean, should additional information be printed.
//' @param alpha Numeric, alpha for transmission model.
// [[Rcpp::export]]
List netinf_(IntegerVector node_ids, CharacterVector node_names,
            List cascade_ids, List cascade_times, int model = 0, 
            double alpha = 1.0, int n_iter = 5, bool verbose = true) {
    
    int n_nodes = node_ids.size();
    int n_cascades = cascade_ids.size();
    
    // Initialize TNIB
    TNetInfBs NIB(false, false);
    TStr node_name("1");
    
    // Add nodes
    for(int i = 0; i < n_nodes; i++) {
        TStr node_name(node_names[i]);
        NIB.AddNodeNm(node_ids[i], TNodeInfo(node_name, 0));
    }
    
    // Add cascades
    for(int i = 0; i < n_cascades; i++) {
        IntegerVector ids = cascade_ids[i];
        NumericVector times = cascade_times[i];
        int cascade_size = ids.size();
        TCascade cascade(alpha, model);
            
        for(int j = 0; j < cascade_size; j++) {
           cascade.Add(ids[j], times[j]);
        }
        cascade.Sort();
        NIB.CascV.Add(cascade);
    }
    
    // Run netinf
    NIB.Init();
    if(verbose) {
        Rcout << "Number nodes: " << NIB.GetNodes() << "\n";
        Rcout << "Number cascades: " << NIB.GetCascs() << "\n";
        Rcout << "Number of potential edges: " << NIB.CascPerEdge.Len() << "\n";
        Rcout << "Running netinf...\n";       
    }

    NIB.GreedyOpt(n_iter);   
    
    // Convert output to Rcpp objects and return
    List edges_out = List::create();
    for (TNGraph::TEdgeI EI = NIB.Graph->BegEI(); EI < NIB.Graph->EndEI(); EI++) {
        IntegerVector edge = IntegerVector::create(EI.GetSrcNId(), 
                                                   EI.GetDstNId());
        edges_out.push_back(edge);
    }
    
    return edges_out;
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
