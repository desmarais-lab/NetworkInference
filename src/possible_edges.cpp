#include <Rcpp.h>
#include <array>
#include "possible_edges.h"

using namespace Rcpp;

edge_map get_possible_edges_(List &cascade_nodes, List &cascade_times) {
    
    edge_map possible_edges;
    int n_cascades = cascade_nodes.size();
    for(int c = 0; c < n_cascades; c++) {
        checkUserInterrupt();
        IntegerVector this_cascade_nodes = cascade_nodes[c];
        NumericVector this_cascade_times = cascade_times[c];
        int csize = this_cascade_nodes.size();
        
        // Use the fact that the cascade data is ordered (see cascade.R)
        for(int i = 0; i < csize; i++) {
            int u = this_cascade_nodes[i];
            double tu = this_cascade_times[i];
            for(int j = i + 1; j < csize; j++) {
                int v = this_cascade_nodes[j];
                double tv = this_cascade_times[j];
                
                // If times are tied skip this combination
                if(tu >= tv) {
                    continue;
                }
                
                // Check if pair is in pair collection. If not include
                std::array<int, 2> pair_id = {{u, v}};
                
                auto it = possible_edges.find(pair_id);
                if(it == possible_edges.end()) {
                    std::vector<int> value;
                    value.push_back(c);
                    possible_edges.insert(make_pair(pair_id, value));
                } else {
                    it->second.push_back(c);
                }
            }
        }
    }
    return possible_edges;
}

//[[Rcpp::export]]
int count_possible_edges_(List &cascade_nodes, List &cascade_times) {
    edge_map edges = get_possible_edges_(cascade_nodes, cascade_times);
    return edges.size();
}
