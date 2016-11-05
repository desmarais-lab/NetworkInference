library(NetworkInference)

context("Test if core netinf method works")

test_that("netinf produces the edges as original netinf executable", {
    data(example_cascades)
    from_netinf <- netinf(node_ids = example_cascades$node_ids, 
                          node_names = example_cascades$node_names, 
                          cascade_ids = example_cascades$cascade_ids, 
                          cascade_times = example_cascades$cascade_times, 
                          alpha = 1, trans_mod = "exponential")
    
    # Results from orignal netinf executable 
    original_edges <- matrix(c(23, 0, 0, 31, 9, 5, 5, 3, 5, 14), nc = 2,
                             byrow = TRUE)
 
    expect_equal(original_edges, from_netinf)
})
