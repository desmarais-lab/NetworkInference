library(NetworkInference)

context("Test if core netinf method works")

data(cascades)
data(validation)
test_that("netinf produces the edges as original netinf executable.", {
    from_netinf <- netinf(cascades, params = 1, trans_mod = "exponential",
                          n_edges = 5, quiet = TRUE)
    t1 <- from_netinf[order(from_netinf[, 1], from_netinf[, 2]), c(-3, -4)]
    rownames(t1) <- c(1:nrow(t1))
    t2 <- validation[order(validation[, 1], validation[, 2]), -c(3:6)]
    rownames(t2) <- c(1:nrow(t2))
    class(t2) <- c("diffnet", "data.frame")
    expect_equal(t1, t2)
})