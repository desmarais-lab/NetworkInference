// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include "netinf_utilities.h"
#include "vuong_test.h"
#include "possible_edges.h"
#include "spanning_tree.h"
#include "netinf.h"
#include "distributions.h"

using namespace Rcpp;

typedef edge_map::iterator m_iter;
typedef edge_map::reverse_iterator rm_iter;

// [[Rcpp::export]]
List netinf_(List &cascade_nodes, List &cascade_times, int &n_edges, 
             std::string &model, NumericVector &params, 
             bool quiet, bool &auto_edges, double &cutoff) {
    
    // Prepare the trees of each cascade (find the optimal spanning tree and 
    // store parents for each node and respective scores)
    if(!quiet) Rcout << "Initializing trees...\n";
    List trees_data = initialize_trees(cascade_nodes, cascade_times, params, 
                                       model);
    List trees = trees_data[0];
    NumericVector tree_scores = trees_data[1];
    
    // Get edges that are possible given the cascade data
    edge_map possible_edges = get_possible_edges_(cascade_nodes, cascade_times,
                                                  quiet);
   
    // Output containers
    int n_p_edges = possible_edges.size();
    if(auto_edges) n_edges = n_p_edges;
    List edges; 
    NumericVector scores;
    NumericVector p_values;
    
    if(n_edges > n_p_edges) {
        std::string msg = "Argument `n_edges` exceeds the maximal number of possible edges (which is " +
            std::to_string(n_p_edges) + ").\n";
        throw std::invalid_argument(msg);
    }
   
    if(!quiet) {
        if(auto_edges) Rcout << "Inferring edges using p-value cutoff...\n";
        else Rcout << "Inferring " << n_edges << " edges...\n";
    }
    
    // Set up for timing first iteration and progress bar (if not auto edges)
    bool show_progress = true;
    if(quiet) show_progress = false;
    if(auto_edges) show_progress = false;
    Progress p(n_edges, show_progress);
    
    int e;
    int check_interval = (n_p_edges / 10) + 1;
    id_array previous_best_edge = {{-1, -1}};
    NumericVector improvements(n_p_edges);
    
    for(e = 0; e < n_edges; e++) {
        
        m_iter start_iter = possible_edges.begin();
        id_array end_id = possible_edges.rbegin()->first;
        if(e > 0) {
            // Find the first edge in possible_edges that has the same child as 
            // previous_best_edge by iterating back from previous_best_edge
            m_iter it_best = possible_edges.find(previous_best_edge);
            int current_child = previous_best_edge[0];
            id_array last_key = it_best->first;
            for(m_iter rit = it_best; rit->first[0] == current_child; rit--) {
                last_key = rit->first;
                if(rit == possible_edges.begin()) break;
            }
            
            // And store the iterator as start point for edge inference 
            start_iter = possible_edges.find(last_key);
            
            // Find the last edge in possible_edges with the same child as 
            // previous_best_edges
            for(m_iter rit = it_best; rit != possible_edges.end(); rit++) {
                if(rit->first[0] != current_child) break;
                last_key = rit->first;
            }
            // And store the iterator as start point for edge inference 
            end_id = possible_edges.find(last_key)->first;
        } else {
            // In the first iteration we have to check every edge
        }
        
        int i = 0;
        for (m_iter x=start_iter; x!=possible_edges.end(); x++) {
            
            // Skip edge inferred in last iteration 
            if(x->first == previous_best_edge) continue;
           
            //potential parent
            int parent = x->first[1];
            // infected node
            int child = x->first[0];
            
            //find replacements for u->v edge
            List edge_replacements = tree_replacement(parent, child, 
                                                      possible_edges, 
                                                      cascade_times, 
                                                      cascade_nodes, trees, 
                                                      model, params);
            // Store the updated potential improvement value for this edge
            x->second.second = edge_replacements[0];
            
            // Check for user interrupt and update progress bar
            if((i % check_interval) == 0) {
                checkUserInterrupt();
            }
            i += 1;
            
            // Stop when last edge to check is reached 
            if(x->first == end_id) break;
        }
        
        // Erase the previous best edge from possible edges
        possible_edges.erase(previous_best_edge);
        
        // Check all improvements to find the best edge
        double max_improvement = 0;
        id_array best_edge;
        for(m_iter x = possible_edges.begin(); x != possible_edges.end(); x++) {
            if(x->second.second >= max_improvement) {
                max_improvement = x->second.second;
                best_edge = x->first;
            }
        }
        // Re calculate the replacement data for the best edge
        List best_edge_replacement_data = tree_replacement(best_edge[1], 
                                                           best_edge[0], 
                                                           possible_edges, 
                                                           cascade_times, 
                                                           cascade_nodes, trees, 
                                                           model, params);
        
        // Store the best results
        // Put edge in order parent->child for backwards compatibility
        id_array best_edge_out = {{best_edge[1], best_edge[0]}};
        edges.push_back(best_edge_out);
        scores.push_back(max_improvement);
        
        // Update the trees with the new edge
        NumericVector old_tree_scores = copy_vector(tree_scores);
        update_trees(trees, tree_scores, best_edge_replacement_data, 
                     cascade_nodes, best_edge);
       
        // Test if the edge improves fit
        double p_value = vuong_test(old_tree_scores, tree_scores);
        p_values.push_back(p_value);

        // Store the best edge for this iteration to inform what to iterate over
        // for the next edge
        previous_best_edge = best_edge;
       
        if(!auto_edges & !quiet) p.increment();
        
        if(!quiet & auto_edges){
            Rcout << "\r" << (e+1) << " edges inferred. P-value: " << 
                p_value << std::flush;         
        } 
        
        if(auto_edges & (p_value >= cutoff)) {
            if(!quiet) Rcout << "\nReached p-value cutoff. Stopping.\n";
            break;
        }
    }
    
    // Write out message if maximum number of edges has been reach below cutoff
    if(auto_edges & (e == n_edges)) {
        if(!quiet) Rcout << "Reached maximum number of possible edges" <<
            " before p-value cutoff.\n";
    }
    List out = List::create(edges, scores, trees, p_values);
    return out;
}

List tree_replacement(int &parent, int &child, edge_map &possible_edges,
                      List &cascade_times, List &cascade_nodes,
                      List &trees, std::string &model, NumericVector &params) {
    
    // Get the cascades the edge is possible in:
    std::array<int, 2> pair_id = {{child, parent}};
    std::vector<int> cascades = possible_edges.find(pair_id)->second.first;
    int n_possible_cascades = cascades.size();
    
    // Initialize output containers
    IntegerVector cascades_with_replacement(n_possible_cascades, -1);
    NumericVector replacement_scores(n_possible_cascades, NA_REAL);
    
    // Total improvement achieved by this edge across all trees
    double improvement = 0;
    for(int c = 0; c < cascades.size(); c++) {
        
        // Get pointers to the data of current cascade
        int this_cascade = cascades[c];
        IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
        NumericVector this_cascade_times = cascade_times[this_cascade];
        
        // Get the event time for u and v in current cascade 
        int idx_parent = get_index(this_cascade_nodes, parent);
        int idx_child = get_index(this_cascade_nodes, child);
        double timing_parent = this_cascade_times[idx_parent];
        double timing_child = this_cascade_times[idx_child];
        
        // extract score associated with the current parent of child
        List this_tree = trees[this_cascade];
        NumericVector scores = this_tree[1];
        double current_score = scores[idx_child];
       
        // what would the score be with the propspective parent (u)
        double replacement_score = edge_score(timing_parent, timing_child, 
                                              model, params, true);
        
        // If the edge has a higher score add it to overall improvement and 
        // store the cascade the improvement occured in (and the new score)
        if(replacement_score > current_score) {
            improvement += replacement_score - current_score; 
            cascades_with_replacement[c] = this_cascade;
            replacement_scores[c] = replacement_score;
        }
    }
   

    List out = List::create(improvement, cascades_with_replacement, 
                            replacement_scores);
    return out;
}

void update_trees(List &trees, NumericVector &tree_scores, 
                  List &replacement_data, List &cascade_nodes, 
                  id_array best_edge) {
   
    IntegerVector updated_cascades = replacement_data[1];
    NumericVector replacement_scores = replacement_data[2];       
    NumericVector old_tree_scores = copy_vector(tree_scores);
    
    // Get u and v of best edge
    int parent = best_edge[1];
    int child = best_edge[0];
    for(int i = 0; i < updated_cascades.size(); i++) {
        int this_cascade = updated_cascades[i];
        if(this_cascade < 0) continue;
        IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
        int idx_child = get_index(this_cascade_nodes, child);
        List casc_tree = trees[this_cascade];

        IntegerVector this_parents = casc_tree[0];
        NumericVector this_scores = casc_tree[1];
        
        //update parent id for v
        this_parents[idx_child] = parent;
        // update branch score
        this_scores[idx_child] = replacement_scores[i];
        tree_scores[this_cascade] = sum_vector(this_scores);
    }
}
