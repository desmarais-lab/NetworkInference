#include <Rcpp.h>
#include "distributions.h"

using namespace Rcpp;

double edge_score(double &event_time_i, double &event_time_j, std::string &model,
                  NumericVector &params, bool tied) {
    double x = event_time_j - event_time_i;
    double y;
    double out;
    if (model == "exponential") {
        y = dexp_(x, params[0]);
    } else if (model == "rayleigh") {
        y = drayleigh_(x, params[0]);
    } else if (model == "log-normal") {
        y = dlognormal_(x, params[0], params[1]);
    } 
    if (tied) {
        out = log(0.5 * y);
    } else {
        out = log(0.0000000001 * y);
    }
    return out;
}

List optimal_spanning_tree(IntegerVector &cascade_nodes, 
                           NumericVector &cascade_times, std::string &model,
                           NumericVector &params) {
 
    // Init containers for the results
    int cascade_size = cascade_nodes.size();
    NumericVector parent_scores(cascade_size);
    IntegerVector parent_ids(cascade_size);
    
    // For each node involved in this cascade find the parent and the weight for
    // the respective edge 
    double tree_score = 0;
    for(int i = 0; i < cascade_size; i++) {
        // Only nodes that have an earlier event time can be parents for current
        // node
        IntegerVector possible_parents;
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
                score = edge_score(parent_times[k], cascade_times[i], model,
                                   params, false);
                if (score > max_parent_score) {
                    max_parent_score = score;
                    parent = possible_parents[k];
                }
            }
            // Select the parent with the max score and store the score
            if(max_parent_score == -INFINITY) {
                std::string msg = "Observed time with zero likelihood. Consider adjusting lambda.\n";
                throw std::invalid_argument(msg);
            }
            parent_ids[i] = parent;
            parent_scores[i] = max_parent_score;
            tree_score += max_parent_score;
            
        // If node can't have parent (fist node in cascade or tied first nodes) 
        // set parent id and score to NA
        } else {
            parent_ids[i] = -1;
            parent_scores[i] = NA_REAL;
        }
    }
    List out = List::create(parent_ids, parent_scores, tree_score); 
    return out; 
}

List initialize_trees(List &cascade_nodes, List &cascade_times, 
                      NumericVector &params, std::string &model) {
    
    // Output container
    int n_cascades = cascade_nodes.size();
    List out(n_cascades);
    NumericVector tree_scores(n_cascades, NA_REAL);
    
    // Calculate optimal spanning tree for each cascade
    for(int i = 0; i < n_cascades; i++) {
        checkUserInterrupt();
        IntegerVector this_cascade_ids = cascade_nodes[i];
        NumericVector this_cascade_times = cascade_times[i];
        List tree_result = optimal_spanning_tree(this_cascade_ids, 
                                                 this_cascade_times, model, 
                                                 params);
        tree_scores[i] = tree_result[2];
        out[i] = tree_result;
    }
    return List::create(out, tree_scores);
}
