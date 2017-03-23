#' Summarize a cascade object
#' 
#' Generates summary statistics for single cascades and across cascades in a 
#' collection, contained in a cascades object.
#' 
#' @param object object of class cascade to be summarized.
#' @param quiet logical, if \code{FALSE} summary stats are printed to std out.
#' @param ... Additional arguments passed to summary.
#' @return Prints cascade summary information to the screen
#'     (if \code{quiet = FALSE}). \code{'# cascades'} is the number of cascades in
#'     the object, \code{'# nodes'} is the number of nodes in the system (nodes 
#'     that can theoretically experience an event), \code{'# nodes in cascades'} is 
#'     the number of unique nodes of the system that experienced an event and 
#'     \code{'# possible edges'} is the number of edges that are possible given
#'     the cascade data (see \code{\link{count_possible_edges}} for details.). 
#'     
#'     Additional summaries for each cascade are returned invisibly. 
#      as a \code{data.frame} with columns \code{name} (name of the 
#'     cascade), \code{length} (length of the cascade as an integer of how many
#'     nodes experienced and event) and \code{n_ties} (number of tied event 
#'     times per cascade).
#'     
#' @examples 
#' data(cascades)
#' summary(cascades)
#' 
#' @export
summary.cascade <- function(object, quiet = FALSE, ...) {
    # Cascade info
    casc_lengths <- sapply(object$cascade_nodes, length)
    dups <- sapply(object$cascade_times, count_dups_)
    names(casc_lengths) <- NULL
    casc_names <- names(object$cascade_nodes)
    casc_info <- data.frame("name" = casc_names, "length" = casc_lengths,
                            "n_ties" = dups)
    # Print out summaries 
    if(!quiet) {
        # Calculate all summaries
        n_nodes <- length(object$node_names)
        n_nodes_in_casc <- length(unique(do.call(c, object$cascade_nodes)))
        npe <- count_possible_edges(object)
        pout <- cbind(summary(casc_lengths), summary(dups))
        colnames(pout) <- c("length", "ties")
        cat(paste0('# cascades: ', length(object$cascade_times), '\n'))
        cat(paste0('# nodes: ', n_nodes, '\n'))
        cat(paste0('# nodes in cascades: ', n_nodes_in_casc, '\n'))
        cat(paste0('# possible edges: ', npe, '\n\n'))
        cat("Summary statistics for cascade length and number of ties:\n") 
        print(pout)
    }
     
    return(invisible(casc_info))
}

count_dups_ <- function(x) {
    return(length(x) - length(unique(x)))
}

