// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <Rcpp.h>
#include <cmath>
#include <string>
#include <array>

// Exponential density
double dexp_(float x, float lambda) {
    return lambda * std::exp(-1 * lambda * x);
}

// Rayleigh density
double drayleigh_(float x, float lambda) {
    return (x / pow(lambda, 2)) * std::exp(-pow(x, 2) / (2 * pow(lambda, 2)));
}

// Calculate the edge weight between two nodes
double edge_weight_(double &event_time_i, double &event_time_j, double &lambda, 
                   double &beta, double &epsilon, bool tied, int &model) {
    double y, out;
    double x = event_time_j - event_time_i;
    if (model == 1) {
        y = dexp_(x, lambda);
    } else if (model == 2) {
        y = drayleigh_(x, lambda);
        out = 0; 
    } else {
        throw std::invalid_argument("Not implemented. Use exponential or rayleigh model\n");
    }
    if (tied) {
        out = log(beta * y);
    } else {
        out = log(epsilon * y);
    }
    return out;
}

// Calculate the optimal spanning tree for a cascade
// Output:
//     List (size 2): 
//         [0] Vector of parent ids, each element indicates the parent of the 
//             node at the same position in the original cascade
//         [1] Vector of scores, each element is the score of the node at this 
//             position in the original data and the node in [0]
Rcpp::List optimal_spanning_tree_(Rcpp::IntegerVector &this_cascade_ids, 
                                 Rcpp::NumericVector &this_cascade_times,
                                 double &lambda, double &beta, double &epsilon,
                                 int &model) {
    
 
    int cascade_size = this_cascade_ids.size();
    
    // Init containers for the results
    Rcpp::NumericVector parent_scores(cascade_size);
    Rcpp::IntegerVector parent_ids(cascade_size);
    
    // For each node involved in this cascade find the parent and the weight for
    // the respective edge 
    
    for(int i = 0; i < cascade_size; i++) {
        // Only nodes that have an earlier event time can be parents for current
        // node
        Rcpp::NumericVector possible_parents;
        Rcpp::NumericVector parent_times;
        for(int j = 0; j < cascade_size; j++) {
            if (this_cascade_times[j] < this_cascade_times[i]) {
                possible_parents.push_back(this_cascade_ids[j]);
                parent_times.push_back(this_cascade_times[j]);
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
                score = edge_weight_(parent_times[k], this_cascade_times[i],
                                            lambda, beta, epsilon, false, model);
                if (score > max_parent_score) {
                    max_parent_score = score;
                    parent = possible_parents[k];
                }
            }
            // Select the parent with the max score and store the score
            parent_ids[i] = parent;
            parent_scores[i] = max_parent_score;
            
        // If node can't have parent (fist node in cascade) set parent id and 
        // score to NA
        } else {
            parent_ids[i] = NA_INTEGER;
            parent_scores[i] = NA_REAL;
        }
    }
    Rcpp::List out = Rcpp::List::create(parent_ids, parent_scores); 
    return out; 
}

// Initialize parents
// Output: 
//     List (size n_cascades):
//         Lists (size 2)
//             [0] parents: vector(size: size of the cascade)
//             [1] scores: vector(size: size of the cascade)
Rcpp::List initialize_parents_(Rcpp::List &cascade_nodes, 
                               Rcpp::List &cascade_times, double &lambda, 
                               double &beta, double &epsilon, int &model,
                               int &n_cascades) {
    
    // Output containers
    Rcpp::List out;
    
    // Calculate optimal spanning tree for each cascade
    for(int i = 0; i < n_cascades; i++) {
        Rcpp::checkUserInterrupt();
        Rcpp::IntegerVector this_cascade_ids = cascade_nodes[i];
        Rcpp::NumericVector this_cascade_times = cascade_times[i];
        Rcpp::List tree_result = optimal_spanning_tree_(this_cascade_ids, 
                                                        this_cascade_times, 
                                                        lambda, beta, epsilon,
                                                        model);
        out.push_back(tree_result);
    }
    return out;
}

// Get index of value (first one that matches) in Rcpp Integer Vector
int which_int_(int value, Rcpp::IntegerVector x) {
    int n = x.size();
    for(int i = 0; i < n; i++) {
        if(x[i] == value) {
            return i;
        }
    }
    return -1; 
}

// Union of two integer vectors with unique elements
void update_children_(Rcpp::IntegerVector &children, 
                      Rcpp::IntegerVector &candidates) {
    int nc = candidates.size();
    for(int i = 0; i < nc; i++) {
        int k = which_int_(candidates[i], children);
        if(k == -1) {
            children.push_back(candidates[i]);
        }
    }
}

// Creates a string pair id from two integer node ids
std::string make_pair_id_(int &u, int &v) {
    return std::to_string(u) + "_" + std::to_string(v);
}


// Find possible edges for each cascade
//
// Returns:
//     A hashmap with pairs as keys ('u_v') and a list as value. The list contains
//     the edge nodes as integers as well as an IntegerVector conatining all 
//     cascades that the edge is possible in
std::map<std::array<int, 2>, std::vector<int> > find_possible_edges_(
        Rcpp::IntegerVector &node_ids, Rcpp::List &cascade_nodes, 
        Rcpp::List &cascade_times, int &n_nodes, int &n_cascades) {
    
    std::map<std::array<int, 2>, std::vector<int> > possible_edges;
    for(int c = 0; c < n_cascades; c++) {
        Rcpp::checkUserInterrupt();
        Rcpp::IntegerVector this_cascade_nodes = cascade_nodes[c];
        Rcpp::NumericVector this_cascade_times = cascade_times[c];
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
int count_possible_edges_(Rcpp::List &cascade_nodes, Rcpp::List &cascade_times) {
   
    int n_cascades = cascade_nodes.size();
    std::map<std::string, int> possible_edges;
    for(int c = 0; c < n_cascades; c++) {
        Rcpp::IntegerVector this_cascade_nodes = cascade_nodes[c];
        Rcpp::NumericVector this_cascade_times = cascade_times[c];
        int csize = this_cascade_nodes.size();
        //Rcpp::Rcout << "Cascade: " << c << ". Size: " << csize << "\n";
        
        // Use the fact that the cascade data is ordered (see cascade.R)
        for(int i = 0; i < csize; i++) {
            Rcpp::checkUserInterrupt();
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
Rcpp::List tree_replacement_(int &n_cascades, int u, int v, 
                             std::map <std::array<int, 2>, std::vector<int> > 
                                 &possible_edges,
                             Rcpp::List &cascade_times, 
                             Rcpp::List &cascade_nodes,
                             Rcpp::List &parent_data, double &lambda, 
                             double &beta, double &epsilon, int &model) {
     
    std::array<int, 2> pair_id = {{u, v}};
    double improvement = 0;
    Rcpp::IntegerVector replacements;
    Rcpp::NumericVector new_scores;
     
    // Get the cascades the edge is possible in:
    std::vector<int> cascades = possible_edges.find(pair_id)->second;
    for(int c = 0; c < cascades.size(); c++) {
        int this_cascade = cascades[c];
        Rcpp::IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
        Rcpp::NumericVector this_cascade_times = cascade_times[this_cascade];
        
        int idx_u = which_int_(u, this_cascade_nodes);
        int idx_v = which_int_(v, this_cascade_nodes);
        double timing_u = this_cascade_times[idx_u];
        double timing_v = this_cascade_times[idx_v];
        
        // extract score associated with the current parent
        Rcpp::List this_parent_data = parent_data[this_cascade];
        Rcpp::NumericVector scores = this_parent_data[1];
        double current_score = scores[idx_v];
        
        // what would the score be with the propspective parent
        double replacement_score = edge_weight_(timing_u, timing_v, lambda, 
                                                beta, epsilon, true, model);
         
        if(replacement_score > current_score) {
            improvement += replacement_score - current_score; 
            replacements.push_back(this_cascade);
            new_scores.push_back(replacement_score);
        }
 
    }
    Rcpp::List out = Rcpp::List::create(improvement, replacements, new_scores);
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
Rcpp::List netinf_(Rcpp::IntegerVector &node_ids, Rcpp::List &cascade_nodes, 
                   Rcpp::List &cascade_times, int &n_edges, int &model, 
                   double &lambda, bool quiet) {
    if(!quiet)
        Rcpp::Rcout << "Initializing...\n";
    int n_cascades = cascade_nodes.size();
    int n_nodes = node_ids.size();
    double beta = 0.5;
    double epsilon = 0.000000001;
    Rcpp::List parent_data = initialize_parents_(cascade_nodes, cascade_times,
                                                 lambda, beta, epsilon, model,
                                                 n_cascades);

    std::map <std::array<int, 2>, std::vector<int> > 
        possible_edges = find_possible_edges_(node_ids, cascade_nodes, 
                                               cascade_times, n_nodes, 
                                               n_cascades);

   
    // Output containers
    Rcpp::List edges(n_edges); 
    Rcpp::NumericVector scores(n_edges);
    
    int n_p_edges = possible_edges.size();
    
    if(n_edges > n_p_edges) {
        std::string msg = "Argument `n_edges` exceeds the maximal number of possible edges (which is " +
            std::to_string(n_p_edges) + ").\n";
        throw std::invalid_argument(msg);
    }
    
    if(!quiet) 
        Rcpp::Rcout << "Inferring Edges...\n";
    Progress p(n_edges*possible_edges.size(), !quiet);
    for(int e = 0; e < n_edges; e++) {
        double max_improvement = 0;
        std::array<int, 2> best_edge;
        Rcpp::List replacement;
        
        for (auto const& x : possible_edges) {
            Rcpp::checkUserInterrupt();
            //potential parent
            int u = x.first[0];
            // infected node
            int v = x.first[1];
            
            std::array<int, 2> this_id = {{u, v}};
            
            //find replacements for u->v edge
            Rcpp::List e_replacements = tree_replacement_(n_cascades, u, v,
                                                          possible_edges, 
                                                          cascade_times, 
                                                          cascade_nodes,
                                                          parent_data, lambda,
                                                          beta, epsilon, model);
            
            // if there is at least one improvement, keep track of edge
            
            double improvement = Rcpp::as<double>(e_replacements[0]);
            if(improvement >= max_improvement) { 
                // store improvement
                max_improvement = improvement;
                // store all replacement information
                replacement = e_replacements;
                // store best edge id
                best_edge = this_id;
            }
        p.increment();
        }
        
        // Store the best results
        edges[e] = best_edge;
        scores[e] = max_improvement;

        // Get data to update parent information for new edge
        Rcpp::IntegerVector replacement_data = replacement[1];
        Rcpp::NumericVector replacement_score = replacement[2];
        
        // Get u and v of best edge
        int u = best_edge[0];
        int v = best_edge[1];
        
        // Update the parent data 
        for(int i = 0; i < replacement_data.size(); i++) {
            int this_cascade = replacement_data[i];
            Rcpp::IntegerVector this_cascade_nodes = cascade_nodes[this_cascade];
            int idx_v = which_int_(v, this_cascade_nodes);
            Rcpp::List casc_tree = parent_data[this_cascade];
            Rcpp::IntegerVector this_parents = casc_tree[0];
            Rcpp::NumericVector this_scores = casc_tree[1];
            //update parent id for v
            this_parents[idx_v] = u;
            // update branch score
            this_scores[idx_v] = replacement_score[i];
            Rcpp::List updated_tree = Rcpp::List::create(this_parents,
                                                         this_scores);
            parent_data[this_cascade] = updated_tree;
        }
        
        // Remove best edge from possible edges
        possible_edges.erase(best_edge);       
    }
    Rcpp::IntegerVector origin(n_edges);
    Rcpp::IntegerVector destination(n_edges);
    Rcpp::List out = Rcpp::List::create(edges, scores);
    return out;
}
