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
        
        for(int i = 0; i < csize; i++) {
            
            int child = this_cascade_nodes[i];
            double t_child = this_cascade_times[i];
           
            std::vector<id_array> edges_child;
            // Use the fact that the cascade data is ordered (see cascade.R) 
            // from each node in the cascade go backwards to find potential 
            // parents for this node.
            for(int j = (i - 1); j >= 0; j--) {
                int parent = this_cascade_nodes[j];
                double t_parent = this_cascade_times[j];
                
                // If times are tied skip this combination
                if(t_child <= t_parent) continue;
                
                // Check if pair is in pair collection. If not include
                id_array pair_id = {{parent, child}};
                edges_child.push_back(pair_id);
                
                auto it = possible_edges.find(pair_id);
                if(it == possible_edges.end()) {
                    std::vector<int> possible_cascades;
                    possible_cascades.push_back(c);
                    std::vector<id_array> dependent_edges;
                    double improvement = -1;
                    edge_map_value value = make_tuple(possible_cascades,
                                                      dependent_edges,
                                                      improvement);
                    possible_edges.insert(make_pair(pair_id, value));
                } else {
                    //First object (possible cascades) in the value of the
                    // map pair
                    std::vector<int>& pc = std::get<0>(it->second);
                    pc.push_back(c);
                }
            }
            // Update the dependent edge data
            for(int k = 0; k < edges_child.size(); k++) {
                id_array this_edge = edges_child[k];
                auto it = possible_edges.find(this_edge);
                for(int j = 0; j < edges_child.size(); j ++) {
                    if(j == k) continue;
                    std::vector<id_array>& x = std::get<1>(it->second);
                    x.push_back(edges_child[j]);
                    //it->second.second.push_back(edges_child[j]);
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
