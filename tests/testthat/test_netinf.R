library(NetworkInference)

context("Test if core netinf method works")

data(cascades)
data(validation)
test_that("netinf produces the edges as original netinf executable.", {
    from_netinf <- netinf(cascades, alpha = 1, trans_mod = "exponential",
                          verbose = FALSE, edge_info = FALSE)
    expect_equal(validation[, -c(3:6)], from_netinf)
})
test_that("netinf produces the same edges and edge information as original.", {
    from_netinf <- netinf(cascades, alpha = 1, trans_mod = "exponential",
                          verbose = FALSE, edge_info = TRUE)
    # Round double precision numbers for comparison
    from_netinf$mean_time_difference <- round(from_netinf$mean_time_difference, 6)
    from_netinf$median_time_difference <- round(from_netinf$median_time_difference, 6)
    from_netinf$marginal_gain <- round(from_netinf$marginal_gain, 6)
    validation$mean_time_difference <- round(validation$mean_time_difference, 6)
    validation$median_time_difference <- round(validation$median_time_difference, 6)
    validation$marginal_gain <- round(validation$marginal_gain, 6)
    expect_equal(validation, from_netinf)
})