library(microbenchmark)
devtools::load_all()

set.seed(123)
df <- simulate_rnd_cascades(100, n_nodes = 50)
cascades <- as_cascade_long(df, node_names = unique(df$node_name))
mean(sapply(cascades$cascade_nodes, length))

microbenchmark('test' = netinf(cascades, trans_mod = "exponential", n_edges = 100, 
             lambda = 0.1, quiet = F), times = 1)
