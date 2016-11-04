library(NetworkInference)

context("Test if core netinf method works")

test_that("netinf produces the edges as original netinf executable", {
    setwd('../../')
    original <- readLines('data/original_output_network.txt')[34:38]
    original <- lapply(original, function(x) as.numeric(unlist(strsplit(x, ','))))
    
    dat <- readLines('data/example-cascades.txt')[34:87]
    cascs <- lapply(dat, function(x) unlist(strsplit(x, ',')))
    ids <- lapply(cascs, function(x) as.numeric(x[seq(1, 86, 2)]))
    times <- lapply(cascs, function(x) as.numeric(x[seq(2, 87, 2)]))
    from_netinf <- netinf(node_ids = c(0:31), 
                          node_names = as.character(c(0:31)), 
                          cascade_ids = ids, cascade_times = times, alpha = 1, 
                          trans_mod = "exponential")
    expect_equal(original, from_netinf)
})