#' Infer latent diffusion network
#' 
#' Infer the most likely latent diffusion model from a set of nodes and 
#' infection times
#' 
#' @importFrom assertthat assert_that
#' 
#' @param node_ids An vector of integer node ids.
#' @param node_names A charcter vector of node names.
#' @param cascade_ids A list of integer vectors containing the node ids of
#'     the cascade in order of infection.
#' @param  cascade_times A list of numeric vectors each containing infection 
#'     times for the correspoinding nodes in \code{cascade_ids}.
#' @param trans_mod character, indicating the choice of model: 
#'      \code{"exponential"}, \code{"power"} (power law) or \code{"rayleigh"}.
#' @param alpha Numeric, alpha for transmission model.
#' @export
netinf <- function(node_ids, node_names, cascade_ids, cascade_times, 
                   trans_mod = "exponential", alpha = 1.0, n_iter = 5) {
    
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
                          alpha = alpha, n_iter = n_iter)
    
    # Return outputs
    return(netinf_out)
}