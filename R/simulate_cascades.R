#' Simulate a set of random cascades
#'
#' Simulate random cascades, for testing and demonstration purposes. No actual 
#' diffusion model is underlying these cascades.
#' 
#' @importFrom stats runif
#' 
#' @param n_cascades Number of cascades to generate.
#' @param n_nodes Number of nodes in the system.
#'     
#' @return A data frame containing (in order of columns) node ids, 
#'     event time and cascade identifier.
#'     
#' @examples
#' 
#' df <- simulate_rnd_cascades(10, n_nodes = 20)
#' head(df)
#' 
#' @export
simulate_rnd_cascades <- function(n_cascades, n_nodes) {
    qassert(n_cascades, "X1[1,)")
    
    make_cascade_ <- function(cid) {
        n <- runif(1, 1, n_nodes)
        ids <- as.character(sample(1:n_nodes, n, replace = FALSE))
        times <- sort(runif(n, 0, 30), decreasing = TRUE)
        return(data.frame(ids, times, rep(cid, n), stringsAsFactors = FALSE))
    }
     
    cascades <- do.call(rbind, lapply(sample(c(1:n_cascades), n_cascades, 
                                             replace = FALSE), make_cascade_))
    colnames(cascades) <- c("node_name", "event_time", "cascade_id")
    rownames(cascades) <- as.character(c(1:nrow(cascades)))
    return(cascades)
}


#' Simulate cascades from a diffusion network
#'
#' Simulate diffusion cascades based on the generative model underlying netinf
#' and a diffusion network.
#' 
#' @import assertthat
#' 
#' @param diffnet object of class \code{diffnet}.
#' @param nsim integer, number of cascades to simulate.
#' @param max_time numeric, the maximum time after which observations are 
#'     censored
#' @param start_probabilities a vector of probabilities for each node in diffnet,
#'     to be the node with the first event. If \code{NULL} a node is drawn from
#'     a uniform distribution over all nodes.
#' @param partial_cascade object of type cascade, containing one partial 
#'     cascades for which further development should be simulated.
#' @param params numeric, (optional) parameters for diffusion time distribution. 
#'    See the details section of \code{\link{netinf}} for specification details.
#'    Only use this argument if parameters different from those contained in the 
#'    \code{diffnet} object should be used or the network is not an object of 
#'    class \code{diffnet}.
#' @param model character, diffusion model to use. One of \code{c("exponential", 
#'     "rayleigh", "log-normal")}. Only use this argument if parameters different 
#'     from those contained in the \code{diffnet} object should be used or the 
#'     network is not an object of class \code{diffnet}.
#' @param nodes vector of node ids if different from nodes included in 
#'     \code{diffnet}
#'     
#' @return A data frame with three columns. Containing 1) The names of 
#'     the nodes (\code{"node_name"}) that experience an event in each cascade, 
#'     2) the event time (\code{"event_time"}) of the corresponding node, 
#'     3) the cascade identifier \code{"cascade_id"}.
#'     
#' @examples
#' 
#' data(cascades) 
#' out <- netinf(cascades, trans_mod = "exponential", n_edges = 5, params = 1)
#' simulated_cascades <- simulate_cascades(out, nsim = 10)
#'  
#' # Simulation from partial cascade
#' 
#' @export
simulate_cascades <- function(diffnet, nsim = 1, max_time = Inf, 
                              start_probabilities = NULL,
                              partial_cascade = NULL, params = NULL, 
                              model = NULL, nodes = NULL) {
    # Check inputs
    assert_that(is.diffnet(diffnet))
    assert_that(nsim >= 1)
    
    if(is.null(model)) {
        model <- attr(diffnet, "diffusion_model")
    }
    if(is.null(params)) {
        params <- attr(diffnet, "diffusion_model_parameters")
    }
    model <- match.arg(model, c("exponential", "rayleigh", "log-normal"))
    if(model == "rayleigh") {
        stop("Rayleigh distribution is not implemented yet. Please choose the exponential or log-normal diffusion model.")
    }
    if(is.null(nodes)) {
        nodes <- unique(c(diffnet$origin_node, diffnet$destination_node)) 
        if(any(!is.element(partial_cascade$cascade_nodes[[1]], nodes))) {
            # TODO: This could be a warning and nodes that are not in the network
            # could be dropped from the partial cascade
            stop("There are nodes in the partial cascade that are not part of the diffusion network. Dropping these nodes. If these nodes should be included for potential out-of-network diffusion, please provide them via the `nodes` argument.")
            #overlap = partial_cascade$cascade_nodes[[1]][
            #    is.element(partial_cascade$cascade_nodes[[1]], nodes)]
            #partial_cascade = subset_cascade(partial_cascade, overlap)
        }
    }
    n_nodes <- length(nodes)
    
    if(!is.null(start_probabilities) & !is.null(partial_cascade)) {
        stop("Start probabilities are not allowed with partial cascades")
    }
    
    if(is.null(start_probabilities)) {
        start_probabilities <- rep(1/n_nodes, n_nodes)
    }
    # Check start probabilities
    qassert(start_probabilities, paste0('N', n_nodes, '[0,1]'))
    
    # Check partial cascade input 
    if(!is.null(partial_cascade)) {
        assert_that(is.cascade(partial_cascade))
        assert_that(length(partial_cascade$cascade_nodes) == 1)
        assert_that(length(partial_cascade$cascade_times) == 1)
        see_if(all(is.element(partial_cascade$cascade_nodes[[1]], nodes)))
    }
    
    ## Create adjacency matrix from edgelist (ordered as in nodes, row -> sender)
    X_ <- matrix(0, ncol = n_nodes, nrow = n_nodes)
    for(k in 1:nrow(diffnet)) {
        i <- which(nodes == diffnet[k, 1])
        j <- which(nodes == diffnet[k, 2])
        X_[i, j] <- 1
    }
    
    sim_out <- lapply(X = 1:nsim, FUN = simulate_cascade_, n_nodes = n_nodes, 
                      params = params, max_time = max_time, 
                      model = model, nodes = nodes, X_ = X_, 
                      partial_cascade = partial_cascade, 
                      start_probabilities = start_probabilities)
    out <- do.call(rbind, sim_out)
    rownames(out) <- NULL
    return(out) 
}


rltruncexp <- function(n, rate, ltrunc) {
    stats::qexp(stats::runif(n, min = stats::pexp(ltrunc, rate = rate), 
                             max = 1), rate = rate)
}

rltrunclnorm <- function(n, meanlog, sdlog, ltrunc) {
    stats::qlnorm(stats::runif(n, min = stats::plnorm(ltrunc, meanlog = meanlog,
                                                      sdlog = sdlog)),
                  meanlog = meanlog, sdlog = sdlog)
}

# This function generates a matrix of relative diffusion times (rows senders,
# columns receivers) where all diffusion times sent by the nodes involved in the
# partial cascade are left truncated to be at least long enough to not infect 
# any nodes before the absolute left censoring time (the last infection time
# in the partial cascade).
truncated_rel_diff_times <- function(n_nodes, nodes, partial_cascade, model, 
                                     params) {
    out <- matrix(stats::rexp(n_nodes^2, rate = params), nrow = n_nodes)
    rownames(out) <- colnames(out) <- nodes
    
    # Get the truncation point for each node in the partial cascade     
    pc_times <- partial_cascade$cascade_times[[1]]
    trunc_points <- pc_times[length(pc_times)] - pc_times
   
    # Truncated draws for nodes in partial cascade 
    pc_nodes = partial_cascade$cascade_nodes[[1]]
    
    if(model == 'exponential') {
        out[pc_nodes, ] = t(sapply(trunc_points, 
                                   function(x) rltruncexp(n_nodes, params, x)))
    } else if(model == "log-normal") {
        out[pc_nodes, ] = t(sapply(trunc_points, 
                                   function(x) rltrunclnorm(n_nodes, params[1], 
                                                            params[2], x)))
    }
    return(out)    
}

# Simulate a single cascade from scratch (random first event node)
simulate_cascade_ <- function(i, nodes, n_nodes, params, max_time, model, X_, 
                              partial_cascade, start_probabilities) {
    beta = 0.5
    epsilon = 10e-9
   
    # Generate relative diffusion times for all pairs
    if(is.null(partial_cascade)) {
        if(model == "exponential") {
            rel_diff_times <- matrix(stats::rexp(n_nodes^2, rate = params), 
                                     nrow = n_nodes)
        } else if(model == "log-normal") {
            rel_diff_times <- matrix(stats::rlnorm(n_nodes^2, meanlog = params[1],
                                                   sdlog = params[2]), 
                                     nrow = n_nodes)       
        }
        rownames(rel_diff_times) <- colnames(rel_diff_times) <- nodes
    } else {
        rel_diff_times <- truncated_rel_diff_times(n_nodes, nodes, 
                                                   partial_cascade, model, 
                                                   params)
    }
   
   
    # No diffusion of node to itself
    diag(rel_diff_times) <- 0
    if(!is.null(partial_cascade)) {
        # No diffusion to nodes that already had an event in partial_cascade
        idx <- which(is.element(nodes, partial_cascade$cascade_nodes[[1]]))
        rel_diff_times[, idx] <- 0       
    }
    # Censor at maximum observation time
    rel_diff_times[rel_diff_times > max_time] <- 0
    
    # Create matrix to transform network for out of network diffusion 
    norm_epsilon <- epsilon / (epsilon + beta)
    Y <- matrix(0, ncol = n_nodes, nrow = n_nodes) 
    # Set colums to 1 with probability norm_ep
    Y[, as.logical(stats::rbinom(n_nodes, 1, prob = norm_epsilon))] <- 1
    
    # Set relative diffusion times between nodes with no corresponding diffusion
    # -edge to 0 
    rel_diff_times <- (X_ - Y)^2 * rel_diff_times
    
    if(is.null(partial_cascade)) {
        start_nodes <- sample(x = nodes, size = 1, prob = start_probabilities)
    } else {
        start_nodes <- nodes[idx]
    }
    # Find shortest path from start node to every other (reachable) node
    if (requireNamespace("igraph", quietly = TRUE)) {
        g <- igraph::graph.adjacency(rel_diff_times, weighted = TRUE, 
                                 mode = "directed")
        dists <- igraph::distances(g, v = start_nodes, mode = "out")
        # reorder
        if(!is.null(partial_cascade)) {
            dists <- dists[partial_cascade$cascade_nodes[[1]], , drop = FALSE]
        }
        
    } else {
        stop("In order to use this functionality the `igraph` package needs to be installed. Run `install.packages('igraph')` and retry.")
    } 
    
    if(is.null(partial_cascade)) {
        prev_event_times <- 0
    } else {
        prev_event_times <- partial_cascade$cascade_times[[1]]
    }
    # Find the earliest event time for each node
    ## Add the event time of each node in partial_cascade to shortest path 
    ## from this node (or add 0 if no partial cascade)
    abs_dists <- t(sapply(1:nrow(dists), 
                          function(j) dists[j, ] + prev_event_times[j]))
    d <- apply(abs_dists, 2, min) 
    d <- d[!is.infinite(d)]
    
    out <- data.frame("node_name" = names(d), 
                      "event_time" = as.numeric(d),
                      "cascade_id" = i)
    return(out)
}

