library(NetworkInference)

context("Test if core netinf method works")

test_that("netinf produces the edges as original netinf executable", {
    data(cascades)
    from_netinf <- netinf(cascades, alpha = 1, trans_mod = "exponential",
                          verbose = FALSE)
    
    # Results from orignal netinf executable 
    original_edges <- as.data.frame(matrix(as.character(c(23, 0, 0, 31, 9, 5, 5, 
                                                          3, 5, 14)), nc = 2, 
                                           byrow = TRUE))
    colnames(original_edges) <- c("origin_node", "destination_node")
 
    expect_equal(original_edges, from_netinf)
})
