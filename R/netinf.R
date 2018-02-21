#' Infer latent diffusion network
#' 
#' Infer a network of diffusion ties from a set of cascades. Each cascade 
#' is defined by pairs of node ids and infection times. 
#' 
#' The algorithm is describe in detail in Gomez-Rodriguez et al. (2010). 
#' Additional information can be found on the 
#' netinf website (\url{http://snap.stanford.edu/netinf/}).
#' 
#' \itemize{
#'     \item Exponential distribution: \code{trans_mod = "exponential"},
#'     \code{params = c(lambda)}. 
#'     Parametrization: \eqn{\lambda e^{-\lambda x}}.
#'     \item Rayleigh distribution: \code{trans_mod = "rayleigh"},
#'     \code{params = c(alpha)}. 
#'     Parametrization: \eqn{\frac{x}{\alpha^2} \frac{e^{-x^2}}{2\alpha^2}}.
#'     \item Log-normal distribution: \code{trans_mod = "log-normal"},
#'     \code{params = c(mu, sigma)}. 
#'     Parametrization: \eqn{\frac{1}{x\sigma\sqrt{2\pi}}e^{-\frac{(ln x - \mu)^2}{2\sigma^2}}}.
#' }
#' 
#' If higher performance is required and for very large data sets, a faster pure C++ 
#' implementation is available in the Stanford Network Analysis Project (SNAP). 
#' The software can be downloaded at \url{http://snap.stanford.edu/netinf/}.
#' 
#' @import checkmate
#' @import assertthat
#' @import data.table
#' 
#' @param  cascades an object of class cascade containing node and cascade 
#'     information. See \code{\link{as_cascade_long}} and 
#'     \code{\link{as_cascade_wide}} for details. 
#' @param trans_mod character, indicating the choice of model: 
#'      \code{"exponential"}, \code{"rayleigh"} or \code{"log-normal"}.
#' @param params numeric, Parameters for diffusion model. If left unspecified (\code{NULL}) 
#'     optimal parameters are inferred using profile maximum likelihood. If
#'     parameters should be fixed see details for how to specify parameters for
#'     the different distributions.
#' @param n_edges integer or numeric, If integer number of edges to infer per
#'     iteration, if a numeric value in the interval (0, 1) (excluding 0 and 1) 
#'     edges are inferred in each iteration until the Vuong test for edge 
#'     addition reaches the p-value of \code{n_edges} or when the maximum 
#'     possible number of edges is reached.
#' @param quiet logical, Should output on progress by suppressed.
#' @param optimize logical, Should the parameters of the diffusion model be 
#'     optimized using approximate profile likelihood. Note that if this option
#'     is used, the network has to be estimated multiple times.
#' @param max_iter, integer, maximum number of iteration for profile likelihood
#'     estimation. Only used if \code{optimize=TRUE}.
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
#' out <- netinf(cascades, trans_mod = "exponential", n_edges = 5, params = 1)
#' 
#' # Starting with a dataframe
#' df <- simulate_rnd_cascades(10, n_nodes = 20)
#' cascades2 <- as_cascade_long(df, node_names = unique(df$node_name))
#' out <- netinf(cascades2, trans_mod = "exponential", n_edges = 5, params = 1)
#' 
#' @export
netinf <- function(cascades, trans_mod = "exponential", n_edges=0.1, 
                   params=NULL, quiet = FALSE, max_iter=5) {
    
    # Check inputs 
    assert_that(class(cascades)[1] == "cascade")
    qassert(trans_mod, "S1")
    # If no number of edges is specified edge selection is automated via Vuong
    # Test
    if(qtest(n_edges, "X1")) {
        qassert(n_edges, "X1[1,)")
        auto_edges <- FALSE 
        cutoff <- 0 # Not used
    } else if(qtest(n_edges, "R1")) {
        qassert(n_edges, "R1(0,1]")
        auto_edges <- TRUE
        cutoff <- n_edges
        n_edges <- 0 # Not used since n_edges inferred form cutoff
    } else stop(paste("n_edges has to be either an integer > 1 or a p-value",
                      "cutoff (0, 1]"))
    
    model <- match.arg(trans_mod, c("exponential", "rayleigh", 'log-normal'))
    
    if(!is.null(params)) {
        if(model == "exponential" | model == "rayleigh") {
            qassert(params, "N1")
        } else if(model == "log-normal") {
            qassert(params[0], "N1")
            qassert(params[1], "N1(0,)")
        }
    }
    
    # Assign integer node ids
    # Note that the ids start at 0 (c++ is 0 indexed)
    node_ids <- c(0:(length(cascades$node_names) - 1))
    names(node_ids) <- cascades$node_names
    
    # Transform node ids in cascades to integer ids
    cascade_nodes <- lapply(cascades$cascade_nodes, function(x) node_ids[x]) 
    
    # Initialize parameters    
    if(is.null(params)) {
        max_times <- sapply(cascades$cascade_times, mean)
        min_times <- sapply(cascades$cascade_times, 
                            function(x) mean(x[-1] - x[-length(x)]))
        if(model == "exponential") {
            lambda_min <- 1 / mean(max_times, na.rm = T)
            lambda_max <- 1 / mean(min_times, na.rm = T)
            params <- mean(c(lambda_max, lambda_min))           
        } else if(model == "rayleigh") {
            N <- length(max_times)
            sh_max <- sqrt(sum(max_times^2) / 2 * N)
            sh_min <- sqrt(sum(min_times^2) / 2 * N)
            adjustment <- exp(lgamma(N) + log(sqrt(N))) / exp(lgamma(N + 1 / 2))
            params <- mean(c(sh_max * adjustment, sh_min * adjustment))
        } else if(model == "log-normal") {
            mean_max <- mean(log(max_times))
            mean_min <- mean(log(min_times))
            sigma_max <- sqrt(var(log(max_times)))
            sigma_min <- sqrt(var(log(min_times)))
            params <- c(mean(mean_max, mean_min), mean(sigma_max, sigma_min))
        }
        if(!quiet) cat('Initialized parameters with: ', params, '\n')
    }
    
    # Run netinf and optimize paramters if required
    df_cascades <- data.table(as.data.frame(cascades))
    setkey(df_cascades, node_name, cascade_id)
    network <- data.frame(origin_node = "", destination_node = "")
    convergence <- FALSE
    i <- 1
    while((i <= max_iter) & (!convergence)) {
        if(!quiet) cat("Iteration", i, "\n")
        netinf_out <- netinf_(cascade_nodes = cascade_nodes, 
                              cascade_times = cascades$cascade_times, 
                              model = model, params = params, n_edges = n_edges, 
                              quiet = quiet, auto_edges = auto_edges, 
                              cutoff = cutoff)
        # Extract the trees and cast them into data frame
        tree_dfs <- lapply(1:length(netinf_out[[3]]), function(i) {
            x <- netinf_out[[3]][[i]]
            out <- as.data.frame(cbind(x[[1]], x[[2]], rep(i, length(x[[1]]))))}
            )
        trees_df <- do.call(rbind, tree_dfs)
        
        # Replace int node ids with node_names 
        trees_df$child <- do.call(c, cascades$cascade_nodes)
        trees_df <- trees_df[!is.na(trees_df[, 2]), ]
        trees_df[, 1] <- cascades$node_names[(trees_df[, 1] + 1)]
        casc_names <- names(cascades$cascade_nodes)
        trees_df[, 3] <- casc_names[trees_df[, 3]] 
        colnames(trees_df) <- c("parent", "log_score", "cascade_id", "child")
        trees_df <- data.table(trees_df) 
        
        # Join trees_df with the event times for each node in each cascade
        # to get diffusion times 
        setkey(trees_df, parent, cascade_id) 
        trees <- trees_df[df_cascades, nomatch=0]
        setnames(trees, "event_time", "parent_time")
        setkey(trees, child, cascade_id) 
        trees <- trees[df_cascades, nomatch=0] 
        setnames(trees, "event_time", "child_time")
        trees$diffusion_time = trees$child_time - trees$parent_time
        
        # Calculate new parameter values based on diffusion times in inferred 
        # trees 
        if(model == "exponential") {
            params = 1 / mean(trees$diffusion_time)
        } else if(model == "rayleigh") {
            params = 
            N <- nrow(trees)
            sh <- sqrt(sum(trees$diffusion_time^2) / 2 * N)
            adjustment <- exp(lgamma(N) + log(sqrt(N))) / exp(lgamma(N + 1 / 2))
            params <- sh * adjustment
        } else if(model == "log-normal") {
            params <- c(mean(log(trees$diffusion_time)), 
                        sqrt(var(log(trees$diffusion_time))))
        }
        new_network <- as.data.frame(cbind(do.call(rbind, netinf_out[[1]]), 
                                     netinf_out[[2]]),
                                     stringsAsFactors = FALSE)
 
        if(identical(new_network, network)){
            convergence = TRUE 
        } 
        network <- new_network
        i = i + 1
    }
    if(!convergence) {
        warning("Reach maximum number of iterations without convergence of the network. Consider increasing max_iter in the netinf call.")
    }
  
    ## Replace integer node_ids with node_names
    ### In the edgelist
    network[, 1] <- cascades$node_names[(network[, 1] + 1)]
    network[, 2] <- cascades$node_names[(network[, 2] + 1)]
    # Backwards compatibility: Flip edges around (comes out of netinf as child, 
    # parent )
    #temp <- network[, 1]
    #network[, 1] <- network[, 2]
    #network[, 2] <- temp
    colnames(network) <- c("origin_node", "destination_node", "improvement")
    network$p_value <- netinf_out[[4]]
    class(network) <- c("diffnet", "data.frame")
    
    return(network) 
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
#' result <- netinf(cascades, n_edges = 6, params = 1)
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
