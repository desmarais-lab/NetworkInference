using namespace Rcpp;

/**
 * Calculate the weighted log likelihood of an edge (i -> j) in a tree.
 * 
 * @param event_time_i Time node i experienced the event
 * @param event_time_j Time node j experienced the event
 * @param model The diffusion model.
 * @param params Parameters of the density funtion of the diffusion 
 *     model.
 * @param tied Is the edge tied in the tree/cascade or is it out of network 
 *     diffusion
 *     
 * @return Weighted log-likelihood score of the edge
 */
double edge_score(double &event_time_i, double &event_time_j, std::string &model,
                  NumericVector &params, bool tied);

/**
 * Generate the optimal spanning tree for a cascade.
 * 
 * @param cascade_nodes Integer vector of node ids in the order in which they 
 *     experienced the event.
 * @param cascade_times The event times for the corresponding nodes in 
 *     cascade_nodes.
 * @param model The diffusion model.
 * @param params Parameters of the density funtion of the diffusion 
 *     model.
 * 
 * @return A list containing two vectors and a scalar: 
 *    [0] Integer vector of parent ids. Each id is the parent of the node in the 
 *    corresponding position of the input `cascade_nodes`
 *    [1] Numeric vector of likelihood scores. Each score is the score for the 
 *    edge form the node in [0] to the node in the respective position in input 
 *    `cascade_nodes`
 *    [2] The total score of the tree (sum of all edge scores)
 */
List optimal_spanning_tree(IntegerVector &cascade_nodes, 
                            NumericVector &cascade_times, std::string &model, 
                            NumericVector &params);

/**
 * Construct the optimal spanning tree for all cascades
 * 
 * @param cascade_nodes List of integer vectors of node ids in the order in 
 *     which they experienced the event in the respective cascade
 * @param cascade_times List of numeric vectors containing the event times 
 *     for the corresponding nodes in cascade_nodes.
 * @param params Parameters of the density funtion of the diffusion 
 *     model.
 * @param model The diffusion model.
 * 
 * @returns A list containing the optimal spanning tree for each cascade (see
 *     optimal spanning tree for the data format of each tree)
 */
List initialize_trees(List &cascade_nodes, List &cascade_times, 
                      NumericVector &params, std::string &model);