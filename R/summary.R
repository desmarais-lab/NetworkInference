#' Summarize cascade object
#' 
#' @param cascades Object of class cascade to be plottet
#' @return Prints cascade summary information to the screen and returns summary
#'     stats in a list
summary.cascade <- function(cascades) {
    # Cascade info
    casc_lengths <- sapply(cascades$cascade_ids, length)
    names(casc_lengths) <- NULL
    casc_names <- names(cascades$cascade_ids)
    casc_info <- data.frame("name" = casc_names, "length" = casc_lengths)
    return(casc_info)
}