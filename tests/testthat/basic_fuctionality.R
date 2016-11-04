library(NetworkInference)

context("Basic functionality test")

test_that("netinf_test produces same file as original netinf executable", {
    original <- readLines('data/original_output_network.txt')
    test_netinf()
    from_test_netinf <- readLines('data/test_out.txt')
    expect_identical(original, from_test_netinf)
})
