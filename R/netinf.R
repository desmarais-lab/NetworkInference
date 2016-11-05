#' Infer latent diffusion network.
#' 
#' Infer the most likely latent diffusion model from a set of nodes and 
#' infection times
#' 
#' The algorithm is describe in detail in Gomez-Rodriguez et al. (2010). 
#' Additional information as well as the C++ source can be found on the 
#' netinf website (\url{http://snap.stanford.edu/netinf/}).' 
#' @importFrom assertthat assert_that
#' 
#' @param node_ids A vector of integer node ids.
#' @param node_names A charcter vector of node names.
#' @param cascade_ids A list of integer vectors containing the node ids of
#'     the cascade in order of infection.
#' @param  cascade_times A list of numeric vectors each containing infection 
#'     times for the correspoinding nodes in \code{cascade_ids}.
#' @param trans_mod character, indicating the choice of model: 
#'      \code{"exponential"}, \code{"power"} (power law) or \code{"rayleigh"}.
#' @param alpha Numeric, alpha for transmission model.
#' @param verbose, logical, should additional output be printed.
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
#' node_ids <- c(1:20)
#' node_names <- as.character(c(1:20))
#' cascade_ids <- list(c(2, 5, 12, 7),
#'                     c(3, 18, 6, 4, 7, 2),
#'                     c(4, 1, 7, 3, 12, 14, 2))
#' cascade_times <- list(c(34.5, 23, 5.67, 0),
#'                       c(23,  20, 14.2, 10, 8.5, 0),
#'                       c(40.1, 34.2, 26.9, 12.5, 10.5, 5, 0))
#' out <- netinf(node_ids = node_ids, node_names = node_names, 
#'               cascade_ids = cascade_ids, cascade_times = cascade_times,
#'               trans_mod = "exponential", alpha = 1, verbose = TRUE)
#'               
#' @export
netinf <- function(node_ids, node_names, cascade_ids, cascade_times, 
                   trans_mod = "exponential", alpha = 1.0, n_iter = 5,
                   verbose = TRUE) {
    
    # Check inputs
    assert_that(is.numeric(node_ids))
    assert_that(is.character(node_names))
    assert_that(is.list(cascade_ids))
    assert_that(is.list(cascade_times))
    assert_that(is.character(trans_mod))
    assert_that(is.numeric(alpha))
    assert_that(is.numeric(n_iter))
    assert_that(length(cascade_ids) == length(cascade_times))
    
    model_char <- match.arg(trans_mod, c("exponential", "power", "rayleigh"))
    if(model_char == "exponential") {
        model <- 0
    } else if(model_char == "power") {
        model <- 1
    } else {
        model <- 2
    }
    
    # Run netinf
    netinf_out <- netinf_(node_ids = node_ids, node_names = node_names, 
                          cascade_ids = cascade_ids, 
                          cascade_times = cascade_times, model = model, 
                          alpha = alpha, n_iter = n_iter, verbose = verbose)
    
    # Return outputs
    return(do.call(rbind, netinf_out))
}