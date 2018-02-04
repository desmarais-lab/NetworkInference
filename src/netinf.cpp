// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include "netinf_utilities.h"
#include "vuong_test.h"
#include "possible_edges.h"
#include "spanning_tree.h"

using namespace Rcpp;

List tree_replacement(int u, int v, edge_map &possible_edges,
                      List &cascade_times, List &cascade_nodes,
                      List &trees, double &lambda, double &beta, 
                      double &epsilon, int &model) {
    
    // Get the cascades the edge is possible in:
    std::array<int, 2> pair_id = {{u, v}};
    std::vector<int> cascades = possible_edges.find(pair_id)->second;
    int n_possible_cascades = cascades.size();
    
    // Initialize output containers
    IntegerVector cascades_with_replacement(n_possible_cascades, NA_INTEGER);
    NumericVector replacement_scores(n_possible_cascades, NA_REAL);
    
    // Total improvement achieved by this edge across all trees
    double improvement = 0;
    for(int c = 0; c < cascades.size(); c++) {
        
        // Get pointers to the data of current cascade
        int this_cascade = cascades[c];
        IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
        NumericVector this_cascade_times = cascade_times[this_cascade];
        
        // Get the event time for u and v in current cascade 
        int idx_u = get_index(this_cascade_nodes, u);
        int idx_v = get_index(this_cascade_nodes, v);
        double timing_u = this_cascade_times[idx_u];
        double timing_v = this_cascade_times[idx_v];
        
        // extract score associated with the current parent of v
        List this_tree = trees[this_cascade];
        NumericVector scores = this_tree[1];
        double current_score = scores[idx_v];
       
        // what would the score be with the propspective parent (u)
        double replacement_score = edge_score(timing_u, timing_v, lambda, 
                                               beta, epsilon, true, model);
        
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

// [[Rcpp::export]]
List netinf_(List &cascade_nodes, List &cascade_times, int &n_edges, int &model,  double &lambda, 
             bool quiet, bool &auto_edges, double &cutoff) {
    
    if(!quiet)
        Rcout << "Initializing...\n";
    int n_cascades = cascade_nodes.size();
    double beta = 0.5;
    double epsilon = 0.000000001;
    
    // Prepare the trees of each cascade (find the optimal spanning tree and 
    // store parents for each node and respective scores)
    List trees = initialize_trees(cascade_nodes, cascade_times, lambda, beta, 
                                  epsilon, model);
    
    // Get the log likelihood of each of the initialized trees
    NumericVector tree_scores(n_cascades, NA_REAL);
    for(int c = 0; c < n_cascades; c++) {
       List this_tree = trees[c];
       NumericVector this_scores = this_tree[1];
       tree_scores[c] = sum_vector(this_scores);
    }
    
    // Get edges that are possible given the cascade data
    edge_map possible_edges = get_possible_edges_(cascade_nodes, cascade_times);
   
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
        else Rcout << "Inferring edges...\n";
    }
    
    // Set up for timing first iteration and progress bar (if not auto edges)
    typedef std::chrono::high_resolution_clock Clock;
    auto t1 = Clock::now();
    bool show_progress = true;
    if(quiet) show_progress = false;
    if(auto_edges) show_progress = false;
    Progress p((n_edges - 1) * possible_edges.size(), show_progress);
    
    int e;
    for(e = 0; e < n_edges; e++) {
        double max_improvement = 0;
        std::array<int, 2> best_edge;
        List best_edge_replacement_data;
        for (auto const& x : possible_edges) {
            checkUserInterrupt();
            //potential parent
            int u = x.first[0];
            // infected node
            int v = x.first[1];
            
            std::array<int, 2> this_id = {{u, v}};
            
            //find replacements for u->v edge
            List edge_replacements = tree_replacement(u, v, possible_edges, 
                                                      cascade_times, 
                                                      cascade_nodes,
                                                      trees, lambda, beta, 
                                                      epsilon, model);
           
            // if there is at least one improvement, keep track of edge
            double improvement = as<double>(edge_replacements[0]);
            if(improvement >= max_improvement) { 
                // store improvement
                max_improvement = improvement;
                // store all replacement information
                best_edge_replacement_data = edge_replacements;
                // store best edge id
                best_edge = this_id;
            }
            if((e > 0) & !quiet)
                if(!auto_edges) p.increment();
        }
       
        // Store the best results
        edges.push_back(best_edge);
        scores.push_back(max_improvement);
        
        // Test if the edge improves fit
        //   Get the tree likelihood scores with the updated edge
        NumericVector tree_scores_after = copy_vector(tree_scores);
        IntegerVector updated_cascades = best_edge_replacement_data[1];
        NumericVector replacement_scores = best_edge_replacement_data[2];
        for(int i = 0; i < updated_cascades.size(); i++) {
            int c = updated_cascades[i];
            if(c < 0) continue;
            tree_scores_after[c] = replacement_scores[i];
        }
        double p_value = vuong_test(tree_scores, tree_scores_after);
        p_values.push_back(p_value);
        tree_scores = tree_scores_after;

        // Get data to update parent information for new edge
       
        // Get u and v of best edge
        int u = best_edge[0];
        int v = best_edge[1];

        // Update the trees 
        for(int i = 0; i < updated_cascades.size(); i++) {
            int this_cascade = updated_cascades[i];
            if(this_cascade < 0) {
                continue;
            }
            IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
            int idx_v = get_index(this_cascade_nodes, v);
            List casc_tree = trees[this_cascade];

            IntegerVector this_parents = casc_tree[0];
            NumericVector this_scores = casc_tree[1];
            
            //update parent id for v
            this_parents[idx_v] = u;
            // update branch score
            this_scores[idx_v] = replacement_scores[i];
            
            List updated_tree = List::create(this_parents, this_scores);
            trees[this_cascade] = updated_tree;
        }
       
        // Remove best edge from possible edges
        possible_edges.erase(best_edge);       
        
        // In the first iteration give an estimate for how long estimation will
        // take
        if (!quiet) {
            if (e == 0) {
                auto t2 = Clock::now();
                std::chrono::duration<double, std::milli> fp_ms = t2 - t1;
                print_time_estimate(fp_ms, auto_edges, n_edges);
            }           
        }
        if(!quiet & auto_edges){
            Rcout << (e+1) << " edges inferred. P-value: " << 
                p_value << "\n";         
        } 
        if(auto_edges & (p_value >= cutoff)) {
            if(!quiet) Rcout << "Reached p-value cutoff. Stopping.\n";
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