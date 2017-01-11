#include <Rcpp.h>
#include <cmath>
#include <string>

// Exponential density
double dexp_(float x, float lambda = 1) {
    return lambda * std::exp(-1 * lambda * x);
}

// Calculate the edge weight between two nodes
double edge_weight_(double event_time_i, double event_time_j, double lambda, 
                   double beta = 0.5, double epsilon = 0.00000000001, 
                   bool tied = false, int model = 1) {
    double out;
    if (model == 1) {
        float x = event_time_j - event_time_i;
        if (tied) {
            out = log(beta * dexp_(x, lambda));
        } else {
            out = log(epsilon * dexp_(x, lambda));
        }
    } else if (model == 2) {
        Rcpp::Rcout << "Not implemented. Use exponential model\n";
        out = 0; 
    } else {
        Rcpp::Rcout << "Not implemented. Use exponential model\n";
        out = 0;
    }
    
    return out;
}

// Calculate the optimal spanning tree for a cascade
Rcpp::List optimal_spanning_tree_(Rcpp::IntegerVector this_cascade_ids, 
                                 Rcpp::NumericVector this_cascade_times,
                                 double lambda, double beta, 
                                 double epsilon) {
    
 
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
                double score = edge_weight_(parent_times[k], this_cascade_times[i],
                                     lambda = lambda, beta = beta, 
                                     epsilon = epsilon);
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
Rcpp::List initialize_parents_(Rcpp::List &cascade_nodes, 
                               Rcpp::List &cascade_times, double &lambda, 
                               double &beta, double &epsilon,
                               int &n_cascades) {
    
    // Output containers
    Rcpp::List parents;
    Rcpp::List scores;
    
    // Calculate optimal spanning tree for each cascade
    for(int i = 0; i < n_cascades; i++) {
        Rcpp::IntegerVector this_cascade_ids = cascade_nodes[i];
        Rcpp::NumericVector this_cascade_times = cascade_times[i];
        Rcpp::List tree_result = optimal_spanning_tree_(
            this_cascade_ids = this_cascade_ids, 
            this_cascade_times = this_cascade_times, lambda = lambda, 
            beta = beta, epsilon = epsilon);
        parents.push_back(tree_result[0]);
        scores.push_back(tree_result[1]);
    }
    
    Rcpp::List out = Rcpp::List::create(parents, scores);
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

// Find unique possible edges for complete data
Rcpp::List unique_possible_edges_(Rcpp::IntegerVector &node_ids, 
                                  Rcpp::List &cascade_nodes, 
                                  int &n_nodes, int n_cascades) {
    
    Rcpp::List out; 
    
    // Get all potential edges (an edge u->v is possible if in at least one case
    // u has an event before v). Generates a list with one element for each node
    // (in order) containing all potential child nodes for this node
    for(int u = 0; u < n_nodes; u++) {
        
        Rcpp::IntegerVector children;
        
        for(int i = 0; i < n_cascades; i++) {
            Rcpp::IntegerVector this_cascade_nodes = cascade_nodes[i];
            int csize = this_cascade_nodes.size();
            int j = which_int_(u, cascade_nodes[i]);
            if(j == -1) {
                continue;
            }
            if(j == (csize - 1)) {
                continue;
            }
            Rcpp::IntegerVector candidates = this_cascade_nodes[Rcpp::seq((j + 1), (csize - 1))];
            update_children_(children, candidates);
        }
        out.push_back(children);
    }
    return out;
}


// Find possible edges for each cascade
Rcpp::List find_possible_edges_(Rcpp::IntegerVector &node_ids, 
                                Rcpp::List &cascade_nodes, 
                                int &n_nodes, int n_cascades) {
    
    Rcpp::List out; 
    
    // Get all potential edges (an edge u->v is possible if in at least one case
    // u has an event before v). Generates a list with one element for each node
    // (in order) containing all potential child nodes for this node
    for(int c = 0; c < n_cascades; c++) {
        
        Rcpp::List pairs;
        Rcpp::IntegerVector this_cascade_nodes = cascade_nodes[c];
        int csize = this_cascade_nodes.size();
        
        // Use the fact that the cascade data is ordered (see cascade.R)
        for(int i = 0; i < csize; i++) {
            for(int j = i + 1; j < csize; j++) {
                Rcpp::IntegerVector pair = Rcpp::IntegerVector::create(
                    this_cascade_nodes[i], this_cascade_nodes[j]);
                pairs.push_back(pair);
            }
        }
        out.push_back(pairs);
    }
    return out;
}
   

//' Run the netinf algorithm on a set of nodes and cascades
//' 
//' @param node_ids An integer vector of integer node ids.
//' @param cascade_nodes A list of integer vectors containing the node ids of
//'     the cascade in order of infection.
//' @param  cascade_times A list of numeric vectors each containing infection 
//'     times for the corresponding nodes in \code{cascade_ids}.
//' @param model integer indicating the choice of model: 0: exponential, 
//'     1: power law, 2: rayleigh.
//' @param lambda Numeric, rate parameter for exponential transmission model.
//' @param n_iter Numeric, number of iterations for optimization.
//' @param verbose boolean, should additional information be printed.
//' @param edge_info boolean, should addditional edge information be returned
//' 
//' @return List containing one vector per edge.
// [[Rcpp::export]]
Rcpp::List netinf_(Rcpp::IntegerVector node_ids, Rcpp::List cascade_nodes, 
                   Rcpp::List cascade_times, int n_edges, int model = 0, 
                   double lambda = 1.0) {
    
    int n_cascades = cascade_nodes.size();
    int n_nodes = node_ids.size();
    double beta = 0.5;
    double epsilon = 0.000000001;
    Rcpp::List parent_data = initialize_parents_(cascade_nodes = cascade_nodes,
                                                 cascade_times = cascade_times,
                                                 lambda = lambda, beta = beta,
                                                 epsilon = epsilon, 
                                                 n_cascades = n_cascades);
    Rcpp::List unique_possible_edges = unique_possible_edges_(
        node_ids = node_ids, cascade_nodes = cascade_nodes, n_nodes = n_nodes, 
        n_cascades = n_cascades
    );
    Rcpp::List possible_edges = find_possible_edges_(
        node_ids = node_ids, cascade_nodes = cascade_nodes, n_nodes = n_nodes, 
        n_cascades = n_cascades
    );
    
    // Output containers
    Rcpp::List edges; 
    Rcpp::NumericVector scores;
    
    for(int e = 0; e < n_edges; e++) {
        continue;   
    }
    
    return possible_edges;
}