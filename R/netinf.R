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
#' 
#' @param  cascades an object of class cascade containing node and cascade 
#'     information. See \code{\link{as_cascade_long}} and 
#'     \code{\link{as_cascade_wide}} for details. 
#' @param trans_mod character, indicating the choice of model: 
#'      \code{"exponential"}, \code{"rayleigh"} or \code{"log-normal"}.
#' @param params numeric, Parameters for diffusion model. If left unspecified 
#'     reasonable parameters are inferred from the data. See details for how to 
#'     specify parameters for the different distributions.
#' @param n_edges integer, number of edges to infer. Leave unspecified if using 
#'     \code{p_value_cutoff}.
#' @param p_value_cutoff numeric, in the interval (0, 1). If 
#'     specified, edges are inferred in each iteration until the Vuong test for 
#'     edge addition reaches the p-value cutoff or when the maximum 
#'     possible number of edges is reached. Leave unspecified if using 
#'     \code{n_edges} to explicitly specify number of edges to infer.
#' @param quiet logical, Should output on progress by suppressed.
#' @param trees logical, Should the inferred cascade trees be returned. Note, 
#'     that this will lead to a different the structure of the function output. 
#'     See section Value for details.
#' 
#' @return Returns the inferred diffusion network as an edgelist in an object of 
#'     class \code{diffnet} and \code{\link[base]{data.frame}}. The first 
#'     column contains the sender, the second column the receiver node. The 
#'     third column contains the improvement in fit from adding the edge that is
#'     represented by the row. The output additionally has the following 
#'     attributes:
#'     \itemize{
#'         \item \code{"diffusion_model"}: The diffusion model used to infer the 
#'             diffusion network.
#'         \item \code{"diffusion_model_parameters"}: The parameters for the 
#'             model that have been inferred by the approximate profile MLE 
#'             procedure.
#'     }
#'     If the argument \code{trees} is set to \code{TRUE}, the output is a list
#'     with the first element being the \code{data.frame} described above, and 
#'     the second element being the trees in edge-list form in a single 
#'     \code{data.frame}.
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
netinf <- function(cascades, trans_mod = "exponential", n_edges = NULL, 
                   p_value_cutoff = NULL, params = NULL, quiet = FALSE, 
                   trees = FALSE) {
    
    # Check inputs 
    assert_that(class(cascades)[1] == "cascade")
    qassert(trans_mod, "S1")
    if(is.null(n_edges) & is.null(p_value_cutoff)) {
        stop('Please specify either `n_edges` or `p_value_cutoff`.')
    }
    if(!is.null(n_edges) & !is.null(p_value_cutoff)) {
        stop('Please only specify either `n_edges` or `p_value_cutoff`.')
    }
    if(!is.null(n_edges)) {
        qassert(n_edges, "X1[1,)")
        auto_edges <- FALSE 
        cutoff <- 0 # Not used
    } else {
        qassert(p_value_cutoff, "R1(0,1]")
        auto_edges <- TRUE
        cutoff <- p_value_cutoff
        n_edges <- 0 # Not used since n_edges inferred form cutoff
    } 
    
    model <- match.arg(trans_mod, c("exponential", "rayleigh", 'log-normal'))
    
    if(!is.null(params)) {
        if(model == "exponential" | model == "rayleigh") {
            qassert(params, "N1")
        } else if(model == "log-normal") {
            qassert(params[1], "N1")
            qassert(params[2], "N1(0,)")
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
        max_times <- do.call(c, lapply(cascades$cascade_times, 
                                       function(x) x[-1] - x[1]))
        min_times <- do.call(c, lapply(cascades$cascade_times, 
                            function(x) x[-1] - x[-length(x)]))
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
            mean_max <- mean(log(max_times[max_times != 0]))
            mean_min <- mean(log(min_times[min_times != 0]))
            sigma_max <- sqrt(stats::var(log(max_times[max_times != 0])))
            sigma_min <- sqrt(stats::var(log(min_times[min_times != 0])))
            params <- c(mean(mean_max, mean_min), mean(sigma_max, sigma_min))
        }
        qassert(params, "R")
        if(!quiet) cat('Initialized parameters with: ', params, '\n')
    }
    
    # Run netinf 
    netinf_out <- netinf_(cascade_nodes = cascade_nodes, 
                          cascade_times = cascades$cascade_times, 
                          model = model, params = params, n_edges = n_edges, 
                          quiet = quiet, auto_edges = auto_edges, 
                          cutoff = cutoff)
       

    network <- as.data.frame(cbind(do.call(rbind, netinf_out[[1]]), 
                             netinf_out[[2]]), stringsAsFactors = FALSE)

  
    ## Replace integer node_ids with node_names
    ### In the edgelist
    network[, 1] <- cascades$node_names[(network[, 1] + 1)]
    network[, 2] <- cascades$node_names[(network[, 2] + 1)]
    colnames(network) <- c("origin_node", "destination_node", "improvement")
    network$p_value <- netinf_out[[4]]
    class(network) <- c("diffnet", "data.frame")
    attr(network, "diffusion_model") = model
    attr(network, "diffusion_model_parameters") = params
    
    if(trees) {
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
        trees_df <- trees_df[, c(1, 4, 2, 3)]
        return(list('network' = network, 'trees' = trees_df))
    }
    else return(network) 
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