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
#'      \code{"exponential"} or \code{"rayleigh"}.
#' @param lambda Numeric, alpha for transmission model.
#' @param n_edges Numeric, number of edges to infer.
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
#' out <- netinf(cascades, trans_mod = "exponential", n_edges = 5, lambda = 1)
#'               
#' @export
netinf <- function(cascades, trans_mod = "exponential", n_edges, lambda) {
    
    # Check inputs 
    assert_that(class(cascades)[1] == "cascade")
    #qassert(cascades, "L3")
    qassert(trans_mod, "S1")
    qassert(lambda, "R1[0,)")
    qassert(n_edges, "X1[1,)")
    model_char <- match.arg(trans_mod, c("exponential", "rayleigh"))
    if(model_char == "exponential") {
        model <- 1
    } else if(model_char == "rayleigh") {
        model <- 2
    } else {
        model <- 3
    }
    
    # Assign integer node ids
    # Note that the ids start at 0 (c++ is 0 indexed)
    node_ids <- c(0:(length(cascades$node_names) - 1))
    names(node_ids) <- cascades$node_names
    
    # Transform node ids in cascades to integer ids
    cascade_nodes <- lapply(cascades$cascade_nodes, function(x) node_ids[x]) 
    
    # Run netinf
    netinf_out <- netinf_(node_ids = node_ids, cascade_nodes = cascade_nodes, 
                          cascade_times = cascades$cascade_times, model = model, 
                          lambda = lambda, n_edges = n_edges)
    
    # Reformat output 
    out <- as.data.frame(cbind(do.call(rbind, netinf_out[[1]]), netinf_out[[2]]),
                         stringsAsFactors = FALSE)
     
    # Replace integer node_ids with node_names
    out[, 1] <- cascades$node_names[(out[, 1] + 1)] # node ids are 0-indexed
    out[, 2] <- cascades$node_names[(out[, 2] + 1)]
    
    colnames(out) <- c("origin_node", "destination_node", "improvement")
    return(out)
}
