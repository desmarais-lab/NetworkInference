#' Summarize cascade object
#' 
#' @param object Object of class cascade to be summarized.
#' @param ... Additional arguments passed to summary
#' @return Prints cascade summary information to the screen and returns summary
#'     stats in a list
#'     
#' @examples 
#' 
#' data(cascades)
#' summary(cascades)
#' 
#' @export
summary.cascade <- function(object, ...) {
    # Cascade info
    casc_lengths <- sapply(object$cascade_nodes, length)
    names(casc_lengths) <- NULL
    casc_names <- names(object$cascade_nodes)
    casc_info <- data.frame("name" = casc_names, "length" = casc_lengths)
    return(casc_info)
}