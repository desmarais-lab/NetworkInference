#include "possible_edges.h"

using namespace Rcpp;

/**
 * For each cascade the edge u -> v is possible check if it improves fit and 
 * keep track of the ones where it does
 * 
 * @param u Integer id of parent node
 * @param v Integer id of child node
 * @param edge_map containing data on all possible edges
 * @param cascade_nodes A list of integer vectors containing the node ids of
 *     the cascade in order of infection.
 * @param  cascade_times A list of numeric vectors each containing infection 
 *     times for the corresponding nodes in \code{cascade_ids}.
 * 
 */
List tree_replacement(int u, int v, edge_map &possible_edges,
                       List &cascade_times, List &cascade_nodes,
                       List &trees, double &lambda, double &beta, 
                       double &epsilon, int &model);

/**
 * Run the netinf algorithm on a set of nodes and cascades
 * 
 * @param cascade_nodes A list of integer vectors containing the node ids of
 *     the cascade in order of infection.
 * @param  cascade_times A list of numeric vectors each containing infection 
 *     times for the corresponding nodes in \code{cascade_ids}.
 * @param model integer indicating the choice of model: 1: exponential, 
 *     2: power law, 3: rayleigh (only exponential implemented).
 * @param lambda Numeric, rate parameter for exponential transmission model.
 * @param n_edges Integer, number of edges to infer.
 * @param quiet, Boolean, Should output on progress by suppressed.
 * 
 * @return List containing one vector per edge.
*/
List netinf_(List &cascade_nodes, List &cascade_times, 
             int &n_edges, int &model, double &lambda, bool quiet, 
             bool auto_edges, double cutoff);