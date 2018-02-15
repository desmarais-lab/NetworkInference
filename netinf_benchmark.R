library(microbenchmark)
devtools::load_all()

set.seed(123)
df <- simulate_rnd_cascades(10, n_nodes = 400)
cascades <- as_cascade_long(df, node_names = unique(df$node_name))
mean(sapply(cascades$cascade_nodes, length))

#load('../donation_diffusion/analysis/test_cascades.RData')
out = netinf(cascades, trans_mod = "exponential", n_edges = 10, 
                      lambda = 0.1, quiet = F)

microbenchmark(netinf(cascades, trans_mod = "exponential", n_edges = 10, 
                      lambda = 0.1, quiet = F), times = 10)
