// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include <array>
#include "possible_edges.h"

using namespace Rcpp;

edge_map get_possible_edges_(List &cascade_nodes, List &cascade_times, 
                             bool& quiet) {
    edge_map possible_edges;
    int n_cascades = cascade_nodes.size();
    if(!quiet) Rcout << "Getting possible edges...\n";
    Progress p(n_cascades, !quiet);
    for(int c = 0; c < n_cascades; c++) {
        checkUserInterrupt();
        IntegerVector this_cascade_nodes = cascade_nodes[c];
        NumericVector this_cascade_times = cascade_times[c];
        int csize = this_cascade_nodes.size();
        
        // Use the fact that the cascade data is ordered (see cascade.R)
        for(int i = 0; i < csize; i++) {
            int parent = this_cascade_nodes[i];
            double t_parent = this_cascade_times[i];
            for(int j = i + 1; j < csize; j++) {
                int child = this_cascade_nodes[j];
                double t_child = this_cascade_times[j];
                
                // If times are tied skip this combination
                if(t_parent >= t_child) {
                    continue;
                }
                
                // Check if pair is in pair collection. If not include
                std::array<int, 2> pair_id = {{child, parent}};
                
                auto it = possible_edges.find(pair_id);
                if(it == possible_edges.end()) {
                    std::vector<int> possible_cascades;
                    possible_cascades.push_back(c);
                    double improvement = -1;
                    edge_value value = make_pair(possible_cascades, 
                                                 improvement);
                    possible_edges.insert(make_pair(pair_id, value));
                } else {
                    it->second.first.push_back(c);
                }
            }
        }
        p.increment();
    }
    return possible_edges;
}

//[[Rcpp::export]]
int count_possible_edges_(List &cascade_nodes, List &cascade_times, 
                          bool quiet=true) {
    edge_map edges = get_possible_edges_(cascade_nodes, cascade_times, quiet);
    return edges.size();
}
