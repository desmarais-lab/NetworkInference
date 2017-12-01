library(NetworkInference)

context("Test if simulation methods works")

if (requireNamespace("igraph", quietly = TRUE)) {
    data(cascades)
    test_that("Simulation function works.", {
        from_netinf <- netinf(cascades, lambda = 1, trans_mod = "exponential",
                              n_edges = 5, quiet = TRUE)
        out <- simulate_cascades(from_netinf, nsim = 100, seed = 123, max_time = 10, 
                                 lambda = 1, beta = 0.5, epsilon = 10e-9, 
                                 model = "exponential")
        casc <- as_cascade_long(out)
        rec <- netinf(casc, lambda = 1, trans_mod = "exponential", n_edges = 5,
                      quiet = TRUE)
        rec <- rec[order(as.numeric(rec[, 1])), ]
        rec <- rec[order(as.numeric(rec[, 2])), ]
        from_netinf <- from_netinf[order(as.numeric(from_netinf[, 1])), ]
        from_netinf <- from_netinf[order(as.numeric(from_netinf[, 2])), ]
        rownames(rec) <- rownames(from_netinf) <- NULL
        expect_equal(from_netinf[, -3], rec[, -3])
    })
    test_that("Simulation function with partial cascade works.", {
        partial_cascade <- cascades
        partial_cascade$cascade_nodes <- cascades$cascade_nodes[10]
        partial_cascade$cascade_times <- cascades$cascade_times[10]
        from_netinf <- netinf(cascades, lambda = 1, trans_mod = "exponential",
                              n_edges = 100, quiet = TRUE)   
        out <- simulate_cascades(from_netinf, nsim = 100, seed = 123, max_time = 10, 
                                 lambda = 1, beta = 0.5, epsilon = 10e-9, 
                                 model = "exponential", 
                                 partial_cascade = partial_cascade)
        expect_equal(length(unique(out[out$node_name == "22", 2])), 1)
        expect_equal(length(unique(out[out$node_name == "28", 2])), 1)
    })
}