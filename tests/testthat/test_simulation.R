library(NetworkInference)
library(igraph)

context("Test if core netinf method works")

data(cascades)
test_that("Simulation function works.", {
    from_netinf <- netinf(cascades, lambda = 1, trans_mod = "exponential",
                          n_edges = 5)
    out <- simulate_cascades(from_netinf, nsim = 100, seed = 123, max_time = 10, 
                             lambda = 1, beta = 0.5, epsilon = 10e-9, 
                             model = "exponential")
    casc <- as.cascade(out, node_names = c("0", "31", "9", "5", "14", "3", "23"))
    rec <- netinf(casc, lambda = 1, trans_mod = "exponential", n_edges = 5)
    rec <- rec[order(as.numeric(rec[, 1])), ]
    rec <- rec[order(as.numeric(rec[, 2])), ]
    from_netinf <- from_netinf[order(as.numeric(from_netinf[, 1])), ]
    from_netinf <- from_netinf[order(as.numeric(from_netinf[, 2])), ]
    rownames(rec) <- rownames(from_netinf) <- NULL
    expect_equal(from_netinf[, -3], rec[, -3])
})