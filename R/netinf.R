#' Infer latent diffusion network
#' 
#' Infer a network of diffusion ties from a set of cascades. Each cascade 
#' is defined by pairs of node ids and infection times. 
#' 
#' The algorithm is describe in detail in Gomez-Rodriguez et al. (2010). 
#' Additional information can be found on the 
#' netinf website (\url{http://snap.stanford.edu/netinf/}).
#' 
#' If higher performance is required and for very large data sets, a faster pure C++ 
#' implementation is available in the Stanford Network Analysis Project (SNAP). 
#' The software can be downloaded at \url{http://snap.stanford.edu/netinf/}.
#' 
#' @import checkmate
#' @import assertthat
#' 
#' @param  cascades an object of class cascade containing node and cascade 
#'     information. See \code{\link{as_cascade_long}} and 
#'     \code{\link{as_cascade_wide}} for details. 
#' @param trans_mod character, indicating the choice of model: 
#'      \code{"exponential"} or \code{"rayleigh"}.
#' @param lambda numeric, alpha for transmission model.
#' @param n_edges integer, number of edges to infer.
#' @param quiet logical, Should output on progress by suppressed.
#' 
#' @return Returns the inferred diffusion network as an edgelist in an object of 
#'     class \code{diffnet} and \code{\link[base]{data.frame}}. The first 
#'     column contains the sender, the second column the receiver node. The 
#'     third column contains the improvement in fit from adding the edge that is
#'     represented by the row.
#'  
#' @references 
#' M. Gomez-Rodriguez, J. Leskovec, A. Krause. Inferring Networks of Diffusion 
#' and Influence.The 16th ACM SIGKDD Conference on Knowledge Discovery and 
#' Data Mining (KDD), 2010.
#'      
#' @examples 
#' 
#' # Data already in cascades format:
#' data(cascades)
#' out <- netinf(cascades, trans_mod = "exponential", n_edges = 5, lambda = 1)
#' 
#' # Starting with a dataframe
#' df <- simulate_rnd_cascades(10, n_nodes = 20)
#' cascades2 <- as_cascade_long(df, node_names = unique(df$node_name))
#' out <- netinf(cascades2, trans_mod = "exponential", n_edges = 5, lambda = 1)
#' 
#' @export
netinf <- function(cascades, trans_mod = "exponential", n_edges, lambda,
                   quiet = FALSE) {
    
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
                          lambda = lambda, n_edges = n_edges, quiet = quiet)
    
    # Reformat output 
    out <- as.data.frame(cbind(do.call(rbind, netinf_out[[1]]), netinf_out[[2]]),
                         stringsAsFactors = FALSE)
     
    # Replace integer node_ids with node_names
    out[, 1] <- cascades$node_names[(out[, 1] + 1)] # node ids are 0-indexed
    out[, 2] <- cascades$node_names[(out[, 2] + 1)]
    
    colnames(out) <- c("origin_node", "destination_node", "improvement")
    class(out) <- c("diffnet", "data.frame")
    return(out)
}


#' Is the object of class diffnet?
#' 
#' Tests if an object is of class diffnet. The class diffnet is appended to the 
#' object returned by \code{\link{netinf}} for dispatch of appropriate plotting 
#' methods.
#' 
#' @param object the object to be tested.
#' 
#' @return \code{TRUE} if object is a diffnet, \code{FALSE} otherwise.
#' 
#' @examples
#' 
#' data(cascades)
#' result <- netinf(cascades, n_edges = 6, lambda = 1)
#' is.diffnet(result)
#' @export
is.diffnet <- function(object) {
    inherits(object, "diffnet")
}


#' Count the number of possible edges in the dataset
#' 
#' Across all cascades, count the edges that are possible. An edge from node
#' \code{u} to node \code{v}
#' is only possible if in at least one cascade \code{u} experienced an event 
#' before \code{v}.
#' 
#' @param cascades Object of class cascade containing the data.
#' 
#' @return An integer count.
#' 
#' @examples
#' data(cascades)
#' count_possible_edges(cascades)
#' 
#' @export 
count_possible_edges <- function(cascades) {
    # Check inputs
    assert_that(is.cascade(cascades))
    
    # Assign integer node ids
    # Note that the ids start at 0 (c++ is 0 indexed)
    node_ids <- c(0:(length(cascades$node_names) - 1))
    names(node_ids) <- cascades$node_names
    
    # Transform node ids in cascades to integer ids
    cascade_nodes <- lapply(cascades$cascade_nodes, function(x) node_ids[x]) 
 
    n <- count_possible_edges_(cascade_nodes, cascades$cascade_times)
    return(n) 
}
