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
    assert_cascade_consistency_(cascades$cascade_ids, cascades$cascade_times)
    assert_node_info_(cascades$node_ids, cascades$node_names)
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
    
    # Run netinf
    netinf_out <- netinf_(node_ids = cascades$node_ids, 
                          node_names = cascades$node_names, 
                          cascade_ids = cascades$cascade_ids, 
                          cascade_times = cascades$cascade_times, 
                          model = model, alpha = alpha, n_iter = n_iter, 
                          verbose = verbose)
    
    # Return outputs
    return(do.call(rbind, netinf_out))
}