#include <Rcpp.h>
#include <cmath>
#include <string>

//' Exponential density
// [[Rcpp::export]]
double dexp_(float x, float lambda = 1) {
    return lambda * std::exp(-1 * lambda * x);
}

//' Calculate the edge weight between two nodes
// [[Rcpp::export]]
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

// Function to find optimal spanning tree given empty graph and cascade
// 

//optimal_spanning_tree = function(cascade_data,lambda,beta =0.5,epsilon=10^(-9)) {
//# first column of cascade_data is the cascade id
//# second column of cascade_data is the vertex id
//# third column of cascade_data is the infection time
//    
//# keep track of assigned parents
//    parent = rep(NA,nrow(cascade_data))
//# keep track of the branch-wise score associated with each branch in the tree
//        score = rep(NA,nrow(cascade_data))
//# loop over each infected node
//        for(i in 1:nrow(cascade_data)) {
//# posible parents have times preceding the node's infection time
//# note, first infected node(s) not assigned a parent
//            possible_parents = cascade_data[which(cascade_data[,3]<cascade_data[i,3]),1]
//# times of possible parents
//            parent_times = cascade_data[which(cascade_data[,3]<cascade_data[i,3]),3]
//# store the scores associated with each parent
//            parent_scores = numeric(length(possible_parents))
//# stop if there are no possible parents
//            if(length(possible_parents)>0) {
//# loop over each parent
//                for(j in 1:length(parent_scores)) {
//# calculate score for the jth parent
//                    parent_scores[j] = w_c(parent_times[j],cascade_data[i,3],lambda=lambda,beta =0.5,epsilon=10^(-9))
//                }
//# store parent
//                parent[i] = possible_parents[which.max(parent_scores)]
//# store associated score
//                score[i] = parent_scores[which.max(parent_scores)]
//            }
//            
//        }
//# returns a matrix in which the parent id and score is binded to the cascade_data
//        return(cbind(cascade_data,parent,score))
//}





//' Run the netinf algorithm on a set of nodes and cascades
//' 
//' @param node_ids An integer vector of integer node ids.
//' @param cascade_ids A list of integer vectors containing the node ids of
//'     the cascade in order of infection.
//' @param  cascade_times A list of numeric vectors each containing infection 
//'     times for the corresponding nodes in \code{cascade_ids}.
//' @param model integer indicating the choice of model: 0: exponential, 
//'     1: power law, 2: rayleigh.
//' @param alpha Numeric, alpha for transmission model.
//' @param n_iter Numeric, number of iterations for optimization.
//' @param verbose boolean, should additional information be printed.
//' @param edge_info boolean, should addditional edge information be returned
//' 
//' @return List containing one vector per edge.
// [[Rcpp::export]]
Rcpp::List netinf_(Rcpp::IntegerVector node_ids, Rcpp::List cascade_ids, 
                   Rcpp::List cascade_times, int model = 0, double alpha = 1.0, 
                   int n_iter = 5, bool verbose = true, bool edge_info = true) {
    
    int n_nodes = node_ids.size();
    int n_cascades = cascade_ids.size();
    
   
    return 0;
}

//' Calculate the optimal spanning tree for a cascade
// [[Rcpp::export]]
Rcpp::List optimal_spanning_tree_(Rcpp::IntegerVector this_cascade_ids, 
                                 Rcpp::NumericVector this_cascade_times,
                                 double &lambda, double &beta, 
                                 double &epsilon) {
    
 
    int cascade_size = this_cascade_ids.size();
    
    // Init containers for the results
    Rcpp::NumericVector parent_scores(cascade_size);
    Rcpp::NumericVector parent_ids(cascade_size);
    
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
            double score;
            int parent;
            double max_parent_score = - INFINITY;
            for (int k = 0; k < n_parents; i++) {
                score = edge_weight_(parent_times[k], this_cascade_times[i],
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
        // score to -1 to indicate missing value
        } else {
            parent_ids[i] = -1;
            parent_scores[i] = -1;
        }
    }
    Rcpp::List out = Rcpp::List::create(parent_ids, parent_scores); 
    return out; 
}

// Initialize parents
