#' Infer latent diffusion network.
#' 
#' Infer the most likely latent diffusion model from a set of nodes and 
#' infection times
#' 
#' The algorithm is describe in detail in Gomez-Rodriguez et al. (2010). 
#' Additional information as well as the C++ source can be found on the 
#' netinf website (\url{http://snap.stanford.edu/netinf/}).' 
#' 
#' @import checkmate
#' @import assertthat
#' 
#' @param  cascades An object of class cascade containing node and cascade 
#'     information. See \link{as.cascade} for details. 
#' @param trans_mod character, indicating the choice of model: 
#'      \code{"exponential"}, \code{"power"} (power law) or \code{"rayleigh"}.
#' @param alpha Numeric, alpha for transmission model.
#' @param n_iter Numeric, number of iterations for optimization.
#' @param verbose logical, should additional output be printed.
#' @param edge_info logical, should additional information on each edge be 
#'    returned.
#' 
#' @return Returns the inferred diffusion network as an object of class 
#'  \link[base]{data.frame}. If \code{edge_info = FALSE} of dimension \code{k x 2} 
#'      where k is the number of edges. If \code{edge_info = TRUE} additional
#'      columns contain information on volume, marginal gain and time difference
#'      for the respective edge. 
#'  
#' @references 
#' M. Gomez-Rodriguez, J. Leskovec, A. Krause. Inferring Networks of Diffusion 
#' and Influence.The 16th ACM SIGKDD Conference on Knowledge Discovery and 
#' Data Mining (KDD), 2010.
#'      
#' @examples 
#' data(cascades)
#' out <- netinf(cascades, trans_mod = "exponential", alpha = 1, verbose = TRUE)
#'               
#' @export
netinf <- function(cascades, trans_mod = "exponential", alpha = 1.0, n_iter = 5,
                   verbose = TRUE, edge_info = TRUE) {
    
    # Check inputs 
    assert_that(class(cascades)[1] == "cascade")
    #qassert(cascades, "L3")
    qassert(trans_mod, "S1")
    qassert(alpha, "R1[0,)")
    qassert(n_iter, "X1[1,)")
    model_char <- match.arg(trans_mod, c("exponential", "power", "rayleigh"))
    if(model_char == "exponential") {
        model <- 0
    } else if(model_char == "power") {
        model <- 1
    } else {
        model <- 2
    }
    
    # Assign integer node ids
    node_ids <- c(1:length(cascades$node_names))
    names(node_ids) <- cascades$node_names
    
    # Transform node ids in cascades to integer ids
    cascade_nodes <- lapply(cascades$cascade_nodes, function(x) node_ids[x]) 
    
    # Run netinf
    netinf_out <- netinf_(node_ids = node_ids, node_names = cascades$node_names, 
                          cascade_ids = cascade_nodes, 
                          cascade_times = cascades$cascade_times, model = model, 
                          alpha = alpha, n_iter = n_iter, verbose = verbose, 
                          edge_info = edge_info)
    
    # Concatenate and order by origin id 
    out <- as.data.frame(do.call(rbind, netinf_out))
    out <- out[order(out[, 1]), ]
    
    # Replace integer node_ids with node_names
    out[, 1] <- cascades$node_names[out[, 1]]
    out[, 2] <- cascades$node_names[out[, 2]]
    
    if(edge_info) {
        colnames(out) <- c("origin_node", "destination_node", "volume", 
                           "marginal_gain", "median_time_difference",
                           "mean_time_difference")
    } else {
        colnames(out) <- c("origin_node", "destination_node")
    }
    return(out)
}