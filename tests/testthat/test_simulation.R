library(NetworkInference)
library(igraph)

context("Test if core netinf method works")

data(cascades)
test_that("simulation function works.", {
    from_netinf <- netinf(cascades, lambda = 1, trans_mod = "exponential",
                          n_edges = 5)
    out <- simulate_cascades(from_netinf, 100, 123, 10, 1, 0.5, 10e-9, "exponential")
})