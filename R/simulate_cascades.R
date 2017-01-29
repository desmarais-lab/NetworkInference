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
#' Simulate random cascades, for testing and demonstration purposes. No actual 
#' diffusion model is underlying these cascades. Requires the \code{igraph} 
#' package to be loaded.
#' 
#' @import assertthat
#' 
#' @param diffnet Object of class \code{diffnet}.
#' @param nsim Number of cascades to simulate.
#' @param seed Seed for random number generator. 
#' @param lambda Parameter for diffusion time distribution.
#' @param beta Weight for in-network diffusion
#' @param epsilon Weight for out of network diffusion
#' @param model Diffusion model to use. One of \code{c("exponential", 
#'     "rayleigh")}.
#' @param max_time Numeric, the maximum time after which observations are 
#'     censored
#'     
#' @return A data frame containing (in order of columns) node ids, 
#'     event time and cascade identifier.
#'     
#' @examples
#' 
#' 
#' @export
simulate_cascades <- function(diffnet, nsim = 1, seed = NULL, max_time = Inf,
                              lambda, beta, epsilon, model, ...) {
    
    # Check inputs
    assert_that(is.diffnet(diffnet))
    assert_that(nsim >= 1)
    model <- match.arg(model, c("exponential", "rayleigh"))
    
    # Initialize data structures 
    set.seed(seed)
    
    ## Nodes and number of nodes
    nodes <- unique(c(diffnet$origin_node, diffnet$destination_node))
    n_nodes <- length(nodes)
    
    ## Create adjacency matrix from edgelist (ordered as in nodes, row -> sender)
    ## with diffusion edge probabilities (normalized epsilon where no edge)
    norm_epsilon <- epsilon / (epsilon + beta)
    probs <- matrix(rep(norm_epsilon, n_nodes * n_nodes), ncol = n_nodes)
    for(k in 1:nrow(diffnet)) {
        i <- which(nodes == diffnet[k, 1])
        j <- which(nodes == diffnet[k, 2])
        probs[i, j] <- 1
    }
     
    sim_out <- lapply(c(1:nsim), simulate_cascade_, n_nodes = n_nodes, 
                      lambda = lambda, probs = probs, max_time = max_time, 
                      model = model, nodes = nodes)
    out <- do.call(rbind, sim_out)
    rownames(out) <- NULL
    return(out)
}

# Simulate a single cascade
simulate_cascade_ <- function(i, nodes, n_nodes, lambda, probs, max_time, model) {
    if(model == "exponential") {
        rel_diff_times <- matrix(stats::rexp(n_nodes^2, rate = lambda), nrow = n_nodes)
    } else if(model == "rayleigh") {
        stop("Rayleigh distribution is not implemented yet. Please choose the exponential diffusion model.")
    }
    diag(rel_diff_times) <- 0
    rel_diff_times[rel_diff_times > max_time] <- 0
    
    Y <- matrix(stats::rbinom(n_nodes^2, 1, prob = as.numeric(probs)), byrow = FALSE,
                nrow = n_nodes) * rel_diff_times
    rownames(Y) <- colnames(Y) <- nodes
    
    start_node <- sample(nodes, 1)
    g <- igraph::graph.adjacency(Y, weighted=TRUE)
    dists <- igraph::distances(g, v = start_node)
    dists <- dists[, -c(which(dists == Inf))]
    out <- data.frame("node_name" = names(dists), "event_time" = dists,
                      "cascade_id" = i)
    return(out)
}
