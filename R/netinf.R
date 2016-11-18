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
#' @importFrom assertthat assert_that
#' 
#' @param  cascades An object of class cascade containing node and cascade 
#'     information. See \link{as.cascade} for details.
#' @param trans_mod character, indicating the choice of model: 
#'      \code{"exponential"}, \code{"power"} (power law) or \code{"rayleigh"}.
#' @param alpha Numeric, alpha for transmission model.
#' @param n_iter Numeric, number of iterations for optimization.
#' @param verbose logical, should additional output be printed.
#' 
#' @return Returns the inferred diffusion network as an object of class 
#'  \link[base]{matrix} of dimension \code{k x 2} where k is the number of edges.
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
                   verbose = TRUE) {
    
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
    cascade_ids <- lapply(cascades$cascade_ids, function(x) node_ids[x]) 
    
    # Run netinf
    netinf_out <- netinf_(node_ids = node_ids, 
                          node_names = cascades$node_names, 
                          cascade_ids = cascade_ids, 
                          cascade_times = cascades$cascade_times, 
                          model = model, alpha = alpha, n_iter = n_iter, 
                          verbose = verbose)
    
    # Replace integer node_ids with node_names
    netinf_out <- do.call(rbind, netinf_out)
    origin <- cascades$node_names[netinf_out[, 1]]
    destin <- cascades$node_names[netinf_out[, 2]]
    out <- data.frame("origin_node" = origin, "destination_node" = destin) 
    return(out)
}