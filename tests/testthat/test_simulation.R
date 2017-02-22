library(NetworkInference)
library(igraph)

context("Test if core netinf method works")

data(cascades)
test_that("simulation function works.", {
    from_netinf <- netinf(cascades, lambda = 1, trans_mod = "exponential",
                          n_edges = 5)
    out <- simulate_cascades(from_netinf, nsim = 100, seed = 123, max_time = 10, 
                             lambda = 1, beta = 0.5, epsilon = 10e-9, 
                             model = "exponential")
})