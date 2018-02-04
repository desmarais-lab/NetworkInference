// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include <cmath>
#include <string>
#include <array>
#include <chrono>
#include <memory>
#include <math.h>
#include <numeric>
#include "netinf_utilities.h"
#include "vuong_test.h"

using namespace Rcpp;

// Calculate the optimal spanning tree for a cascade
// 
// @param cascade_nodes
// @param cascade_times
// @param lambda
// @param beta
// @param epsilon
// @param model
// Output:
//     List (size 2): 
//         [0] Vector of parent ids, each element indicates the parent of the 
//             node at the same position in the original cascade
//         [1] Vector of scores, each element is the score of the node at this 
//             position in the original data and the node in [0]
List optimal_spanning_tree_(IntegerVector &cascade_nodes, 
                                 NumericVector &cascade_times,
                                 double &lambda, double &beta, double &epsilon,
                                 int &model) {
 
    // Init containers for the results
    int cascade_size = cascade_nodes.size();
    NumericVector parent_scores(cascade_size);
    IntegerVector parent_ids(cascade_size);
    
    // For each node involved in this cascade find the parent and the weight for
    // the respective edge 
    for(int i = 0; i < cascade_size; i++) {
        // Only nodes that have an earlier event time can be parents for current
        // node
        NumericVector possible_parents;
        NumericVector parent_times;
        for(int j = 0; j < cascade_size; j++) {
            if (cascade_times[j] < cascade_times[i]) {
                possible_parents.push_back(cascade_nodes[j]);
                parent_times.push_back(cascade_times[j]);
            } 
        }
        // Find the parent with the highest score if there are possible parents
        int n_parents = possible_parents.size();
        // If there are multiple potential parents find the one that gives the e
        // edge the maximum weight
        if (n_parents > 0) {
            double max_parent_score = -INFINITY;
            int parent;
            double score;
            for (int k = 0; k < n_parents; k++) {
                score = edge_weight_(parent_times[k], cascade_times[i],
                                     lambda, beta, epsilon, false, model);
                if (score > max_parent_score) {
                    max_parent_score = score;
                    parent = possible_parents[k];
                }
            }
            // Select the parent with the max score and store the score
            parent_ids[i] = parent;
            parent_scores[i] = max_parent_score;
            
        // If node can't have parent (fist node in cascade or tied first nodes) 
        // set parent id and score to NA
        } else {
            parent_ids[i] = NA_INTEGER;
            parent_scores[i] = NA_REAL;
        }
    }
    List out = List::create(parent_ids, parent_scores); 
    return out; 
}

// Initialize parents
// Output: 
//     List (size n_cascades):
//         Lists (size 2)
//             [0] parents: vector(size: size of the cascade)
//             [1] scores: vector(size: size of the cascade)
List initialize_parents_(List &cascade_nodes, 
                               List &cascade_times, double &lambda, 
                               double &beta, double &epsilon, int &model,
                               int &n_cascades) {
    
    // Output container
    List out(n_cascades);
    
    // Calculate optimal spanning tree for each cascade
    for(int i = 0; i < n_cascades; i++) {
        checkUserInterrupt();
        IntegerVector this_cascade_ids = cascade_nodes[i];
        NumericVector this_cascade_times = cascade_times[i];
        List tree_result = optimal_spanning_tree_(this_cascade_ids, 
                                                        this_cascade_times, 
                                                        lambda, beta, epsilon,
                                                        model);
        out[i] = tree_result;
    }
    return out;
}

// Find possible edges for each cascade
//
// Returns:
//     A hashmap with pairs as keys ('u_v') and a list as value. The list contains
//     the edge nodes as integers as well as an IntegerVector conatining all 
//     cascades that the edge is possible in
std::map<std::array<int, 2>, std::vector<int> > find_possible_edges_(
        IntegerVector &node_ids, List &cascade_nodes, 
        List &cascade_times, int &n_nodes, int &n_cascades) {
    
    std::map<std::array<int, 2>, std::vector<int> > possible_edges;
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


// [[Rcpp::export]]
int count_possible_edges_(List &cascade_nodes, List &cascade_times) {
   
    int n_cascades = cascade_nodes.size();
    std::map<std::string, int> possible_edges;
    for(int c = 0; c < n_cascades; c++) {
        IntegerVector this_cascade_nodes = cascade_nodes[c];
        NumericVector this_cascade_times = cascade_times[c];
        int csize = this_cascade_nodes.size();
        
        // Use the fact that the cascade data is ordered (see cascade.R)
        for(int i = 0; i < csize; i++) {
            checkUserInterrupt();
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
                std::string pair_id = make_pair_id_(u, v);
                std::map<std::string,int>::iterator it;
                it = possible_edges.find(pair_id);
                if(it == possible_edges.end()) {
                    possible_edges.insert(std::pair<std::string, int>(pair_id, 1));
                } else {
                    it->second += 1;
                }
            }
        }
    }
    return possible_edges.size();
}


// Find potential replacements for edge u->v
List tree_replacement_(int &n_cascades, int u, int v, 
                             std::map <std::array<int, 2>, std::vector<int> > 
                                 &possible_edges,
                             List &cascade_times, 
                             List &cascade_nodes,
                             List &parent_data, double &lambda, 
                             double &beta, double &epsilon, int &model) {

    // Get the cascades the edge is possible in:
    std::array<int, 2> pair_id = {{u, v}};
    std::vector<int> cascades = possible_edges.find(pair_id)->second;
    int n_possible_cascades = cascades.size();
    double improvement = 0;
    IntegerVector replacements(n_possible_cascades);
    for(int i = 0; i < replacements.size(); i++) {
        replacements[i] = -1;
    }
    NumericVector new_scores(n_possible_cascades);

    for(int c = 0; c < cascades.size(); c++) {
       
        int this_cascade = cascades[c];
        IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
        NumericVector this_cascade_times = cascade_times[this_cascade];
       
        int idx_u = which_int_(u, this_cascade_nodes);
        int idx_v = which_int_(v, this_cascade_nodes);
        double timing_u = this_cascade_times[idx_u];
        double timing_v = this_cascade_times[idx_v];
        
        // extract score associated with the current parent
        List this_parent_data = parent_data[this_cascade];
        NumericVector scores = this_parent_data[1];

        double current_score = scores[idx_v];
       
        // what would the score be with the propspective parent
        double replacement_score = edge_weight_(timing_u, timing_v, lambda, 
                                                beta, epsilon, true, model);
        
        if(replacement_score > current_score) {
            improvement += replacement_score - current_score; 
            replacements[c] = this_cascade;
            new_scores[c] = replacement_score;
        }
    }
   
    List out = List::create(improvement, replacements, new_scores);
    return out;
}

// Run the netinf algorithm on a set of nodes and cascades
// 
// @param node_ids An integer vector of integer node ids.
// @param cascade_nodes A list of integer vectors containing the node ids of
//     the cascade in order of infection.
// @param  cascade_times A list of numeric vectors each containing infection 
//     times for the corresponding nodes in \code{cascade_ids}.
// @param model integer indicating the choice of model: 1: exponential, 
//     2: power law, 3: rayleigh (only exponential implemented).
// @param lambda Numeric, rate parameter for exponential transmission model.
// @param n_edges Integer, number of edges to infer.
// @param quiet, Boolean, Should output on progress by suppressed.
// 
// @return List containing one vector per edge.
// [[Rcpp::export]]
List netinf_(IntegerVector &node_ids, List &cascade_nodes, 
                   List &cascade_times, int &n_edges, int &model, 
                   double &lambda, bool quiet, bool auto_edges, double cutoff) {
    if(!quiet)
        Rcout << "Initializing...\n";
    int n_cascades = cascade_nodes.size();
    int n_nodes = node_ids.size();
    double beta = 0.5;
    double epsilon = 0.000000001;
   
    List parent_data = initialize_parents_(cascade_nodes, cascade_times,
                                                 lambda, beta, epsilon, model,
                                                 n_cascades);
    // Get the log likelihood of each of the initialized trees
    NumericVector tree_scores(n_cascades);
    for(int c = 0; c < n_cascades; c++) {
        
       List this_tree = parent_data[c];
       NumericVector this_scores = this_tree[1];
       NumericVector this_parents = this_tree[0];
       tree_scores[c] = sum_vector(this_scores);
    }

    std::map <std::array<int, 2>, std::vector<int> > 
        possible_edges = find_possible_edges_(node_ids, cascade_nodes, 
                                               cascade_times, n_nodes, 
                                               n_cascades);

   
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
    
    // Set up for timing first iteration
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
        List replacement;
    
        for (auto const& x : possible_edges) {
    
            checkUserInterrupt();
            //potential parent
            int u = x.first[0];
            // infected node
            int v = x.first[1];
            
            std::array<int, 2> this_id = {{u, v}};
            
            //find replacements for u->v edge
            List e_replacements = tree_replacement_(n_cascades, u, v,
                                                          possible_edges, 
                                                          cascade_times, 
                                                          cascade_nodes,
                                                          parent_data, lambda,
                                                          beta, epsilon, model);
           
            // if there is at least one improvement, keep track of edge
            double improvement = as<double>(e_replacements[0]);
            if(improvement >= max_improvement) { 
                // store improvement
                max_improvement = improvement;
                // store all replacement information
                replacement = e_replacements;
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
        IntegerVector updated_cascades = replacement[1];
        NumericVector replacement_scores = replacement[2];
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

        // Update the parent data 
        for(int i = 0; i < updated_cascades.size(); i++) {
            int this_cascade = updated_cascades[i];
            if(this_cascade < 0) {
                continue;
            }
            IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
            int idx_v = which_int_(v, this_cascade_nodes);
            List casc_tree = parent_data[this_cascade];

            IntegerVector this_parents = casc_tree[0];
            NumericVector this_scores = casc_tree[1];
            
            //update parent id for v
            this_parents[idx_v] = u;
            // update branch score
            this_scores[idx_v] = replacement_scores[i];
            
            List updated_tree = List::create(this_parents,
                                                         this_scores);
            parent_data[this_cascade] = updated_tree;
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
    List out = List::create(edges, scores, parent_data, p_values);
    return out;
}