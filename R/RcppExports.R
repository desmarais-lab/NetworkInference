# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

netinf_ <- function(cascade_nodes, cascade_times, n_edges, model, params, quiet, auto_edges, cutoff) {
    .Call(`_NetworkInference_netinf_`, cascade_nodes, cascade_times, n_edges, model, params, quiet, auto_edges, cutoff)
}

count_possible_edges_ <- function(cascade_nodes, cascade_times, quiet = TRUE) {
    .Call(`_NetworkInference_count_possible_edges_`, cascade_nodes, cascade_times, quiet)
}

