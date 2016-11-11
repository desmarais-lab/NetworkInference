library(NetworkInference)

context("Test if cascade data structure methods work")

test_that("as.cascade.data.frame produces works", {
    set.seed(123)
    dat <- simulate_cascades_(10)
    out <- as.cascade.data.frame(dat, node_ids = c(0:20))
    expect_equal(length(out), 4)
})