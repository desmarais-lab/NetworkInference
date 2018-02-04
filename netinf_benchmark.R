library(microbenchmark)
load('../donation_diffusion/analysis/test_cascades.RData')

microbenchmark(netinf(cascades, trans_mod = "exponential", n_edges = 1, 
                      lambda = 0.1), times = 1)
