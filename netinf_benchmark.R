library(microbenchmark)
devtools::load_all()

cs = c(10, 50, 100)
ns = c(5, 10, 50)
params = data.frame(expand.grid(cs, ns), t = NA)
i = 1
set.seed(123)
for(c_ in cs) {
    for(n in ns) {
        df <- simulate_rnd_cascades(params[i, 1], n_nodes = params[i, 2])
        cascades <- as_cascade_long(df, node_names = unique(df$node_name))
        mean(sapply(cascades$cascade_nodes, length))
        
        bm = microbenchmark('test' = netinf(cascades, trans_mod = "exponential", 
                                            n_edges = 10, 
                     lambda = 0.1, quiet = T), times = 50)
        t = mean(bm$time) * 1e-6
        params[i, 3] = t
        i = i + 1
        cat('c: ', c_, "n: ", n, 'time:\t', t, ' milliseconds\n', sep=' ')
    }
}

# Current performance (e9558ddac1068beb480a1e3c808c50cf10ce51c1)
Var1 Var2          t
1   10    5   1.313620
2   50    5   1.085694
3  100    5   2.178076
4   10   10   1.742602
5   50   10   5.815067
6  100   10   8.896241
7   10   50  96.222636
8   50   50 202.100437
9  100   50 447.750012
