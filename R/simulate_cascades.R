#' Simulate a set of random cascades
#'
#' Simulate random cascades, for testing and demonstration purposes. No actual 
#' diffusion model is underlying these cascades.
#' 
#' @importFrom stats runif
#' 
#' @param n_cascades Number of cascades to generate.
#' @param n_nodes Number of nodes in the system.
#' @param id_class One of \code{c("character", "factor", "numeric")}. What class
#'     should the cascade_id indicator be. 
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
simulate_rnd_cascades <- function(n_cascades, n_nodes, id_class = "character") {
    qassert(n_cascades, "X1[1,)")
    assert_that(n_nodes <= 26)
    id_class <- match.arg(arg = id_class, choices = c("character", "factor", 
                                                      "numeric"))
    make_cascade_ <- function(cid, id_class) {
        n <- runif(1, 1, n_nodes)
        ids <- sample(letters, n, replace = FALSE)
        times <- sort(runif(n, 0, 30), decreasing = TRUE)
        return(data.frame(ids, times, rep(cid, n), stringsAsFactors = FALSE))
    }
     
    if(id_class == "character"){
        ids <- as.character(outer(letters, letters, FUN = paste0))
        cascades <- do.call(rbind, lapply(sample(ids, n_cascades, 
                                                 replace = FALSE), 
                                          make_cascade_))
    } else if(id_class == "factor") {
        ids <- as.character(outer(letters, letters, FUN = paste0))
        cascades <- do.call(rbind, lapply(sample(ids, n_cascades, 
                                                 replace = FALSE), 
                                          make_cascade_))
        cascades[, 3] <- as.factor(cascades[, 3])
    } else if(id_class == "numeric") {
         cascades <- do.call(rbind, lapply(sample(c(1:n_cascades), n_cascades, 
                                                  replace = FALSE), 
                                          make_cascade_))
    }
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
#' @param seed integer, seed for random number generator. 
#' @param lambda numeric, parameter for diffusion time distribution.
#' @param beta numeric, weight for in-network diffusion
#' @param epsilon numeric, weight for out of network diffusion
#' @param model character, diffusion model to use. One of \code{c("exponential", 
#'     "rayleigh")}.
#' @param max_time numeric, the maximum time after which observations are 
#'     censored
#' @param start_probabilities a vector of probabilities for each node in diffnet,
#'     to be the node with the first event. If \code{NULL} a node is drawn from
#'     a uniform distribution over all nodes.
#' @param partial_cascade object of type cascade, containing one partial 
#'     cascades for which further development should be simulated.
#'     
#' @return A data frame with three columns. Containing 1) The names of 
#'     the nodes (\code{"node_name"}) that experience an event in each cascade, 
#'     2) the event time (\code{"event_time"}) of the corresponding node, 
#'     3) the cascade identifier \code{"cascade_id"}.
#'     
#' @examples
#' 
#' data(cascades) 
#' out <- netinf(cascades, trans_mod = "exponential", n_edges = 5, lambda = 1)
#' simulated_cascades <- simulate_cascades(out, nsim = 10, lambda = 1, 
#'                                         beta = 0.5, epsilon = 10^-9, 
#'                                         model = "exponential")
#'  
#' # Simulation from partial cascade
#' 
#' @export
simulate_cascades <- function(diffnet, nsim = 1, seed = NULL, max_time = Inf,
                              lambda, beta, epsilon, model, 
                              partial_cascade = NULL, 
                              start_probabilities = NULL) {
    
    # Check inputs
    assert_that(is.diffnet(diffnet))
    assert_that(nsim >= 1)
    model <- match.arg(model, c("exponential", "rayleigh"))
    if(model == "rayleigh") {
        stop("Rayleigh distribution is not implemented yet. Please choose the exponential diffusion model.")
    }
    # Extract Nodes and number of nodes
    nodes <- unique(c(diffnet$origin_node, diffnet$destination_node))
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
        assert_that(all(is.element(partial_cascade$cascade_nodes[[1]], nodes)))
    }
    
    set.seed(seed)
   
    ## Create adjacency matrix from edgelist (ordered as in nodes, row -> sender)
    X_ <- matrix(0, ncol = n_nodes, nrow = n_nodes)
    for(k in 1:nrow(diffnet)) {
        i <- which(nodes == diffnet[k, 1])
        j <- which(nodes == diffnet[k, 2])
        X_[i, j] <- 1
    }
    
    sim_out <- lapply(X = 1:nsim, FUN = simulate_cascade_, n_nodes = n_nodes, 
                      lambda = lambda, epsilon = epsilon, max_time = max_time, 
                      model = model, nodes = nodes, beta = beta, X_ = X_, 
                      partial_cascade = partial_cascade, 
                      start_probabilities = start_probabilities)
    out <- do.call(rbind, sim_out)
    rownames(out) <- NULL
    return(out) 
}


# Simulate a single cascade from scratch (random first event node)
simulate_cascade_ <- function(i, nodes, n_nodes, lambda, epsilon, max_time, 
                              model, beta, X_, partial_cascade, 
                              start_probabilities) {
    
    # Generate relative diffusion times for all pairs
    rel_diff_times <- matrix(stats::rexp(n_nodes^2, rate = lambda), nrow = n_nodes)
    
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
    rownames(rel_diff_times) <- colnames(rel_diff_times) <- nodes
    
    if(is.null(partial_cascade)) {
        start_nodes <- sample(x = nodes, size = 1, prob = start_probabilities)
    } else {
        start_nodes <- nodes[idx]
    }
    # Find shortest path from start node to every other (reachable) node
    g <- igraph::graph.adjacency(rel_diff_times, weighted = TRUE, 
                                 mode = "directed")
    dists <- igraph::distances(g, v = start_nodes, mode = "out")
    
    if(is.null(partial_cascade)) {
        prev_event_times <- 0
    } else {
        prev_event_times <- partial_cascade$cascade_times[[1]]
    }
    # Find the earliest event time for each node
    ## Add the event time of each node in partial_cascade to shortest path 
    ## from this node (or add 0 if no partial cascade)
    abs_dists <- t(sapply(1:nrow(dists), 
                          function(i) dists[i, ] + prev_event_times[i]))
    d <- apply(abs_dists, 2, min) 
    d <- d[!is.infinite(d)]
    
    out <- data.frame("node_name" = names(d), 
                      "event_time" = as.numeric(d),
                      "cascade_id" = i)
    return(out)
}
