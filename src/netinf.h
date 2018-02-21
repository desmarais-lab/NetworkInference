#include "possible_edges.h"

using namespace Rcpp;

/**
 * For each cascade the edge u -> v is possible check if it improves fit and 
 * keep track of the ones where it does
 * 
 * @param u Integer id of parent node
 * @param v Integer id of child node
 * @param possible_edges edge_map containing data on all possible edges
 * @param cascade_nodes A list of integer vectors containing the node ids of
 *     the cascade in order of infection.
 * @param  cascade_times A list of numeric vectors each containing infection 
 *     times for the corresponding nodes in \code{cascade_ids}.
 * 
 * @return An Rcpp List containing:
 *     [0]: Aggregate improvement from this edge over all trees
 *     [1]: An integer vector of cascades where the edge caused improvement
 *     [2]: The scores of the edge in each of the cascades in [1]
 */
List tree_replacement(int &u, int &v, edge_map &possible_edges,
                       List &cascade_times, List &cascade_nodes,
                       List &trees, std::string &model, NumericVector &params);

/**
 * Run the netinf algorithm on a set of nodes and cascades
 * 
 * @param cascade_nodes A list of integer vectors containing the node ids of
 *     the cascade in order of infection.
 * @param  cascade_times A list of numeric vectors each containing infection 
 *     times for the corresponding nodes in \code{cascade_ids}.
 * @param model integer indicating the choice of model: 1: exponential, 
 *     2: power law, 3: rayleigh (only exponential implemented).
 * @param params NumericVector, Parameters for transmission model.
 * @param n_edges Integer, number of edges to infer.
 * @param quiet, Boolean, Should output on progress by suppressed.
 * @param cutoff, p-value cutoff if auto-edges=TRUE
 * 
 * @return List containing one vector per edge.
*/
List netinf_(List &cascade_nodes, List &cascade_times, 
             int &n_edges, std::string &model, NumericVector &params, bool quiet, 
             bool auto_edges, double cutoff);

/**
 * Update the trees for each cascade using the new edge
 * 
 * @param trees List of trees for each cascade (see spanning_tree.h for 
 *     documentation of the data structure)
 * @param tree_scores Numeric Vector of aggregate likelihood scores for each tree
 * @param replacement_data Return object from tree_replacement.
 * @param cascade_nodes A list of integer vectors containing the node ids of
 *     the cascade in order of infection.
 * @param best_edge node ids of the edge that's updated
 * @param
 */
void update_trees(List &trees, NumericVector &tree_scores, 
                  List &replacement_data, List &cascade_nodes, 
                  std::array<int, 2> best_edge);