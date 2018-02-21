library(microbenchmark)
devtools::load_all()

cs = c(10, 50, 100, 500)
ns = c(5, 10, 50, 100)
params = data.frame(expand.grid(cs, ns), t = NA)
set.seed(123)
for(i in 1:nrow(params)) {
        df <- simulate_rnd_cascades(params[i, 1], n_nodes = params[i, 2])
        cascades <- as_cascade_long(df, node_names = unique(df$node_name))
        bm = microbenchmark('test' = netinf(cascades, trans_mod = "exponential", 
                                            n_edges = 10, params = 1, 
                                            quiet = T), times = 50)
        netinf(cascades, max_iter = 1, params = 10)
        t = mean(bm$time) * 1e-6
        params[i, 3] = t
}

# Current performance (4a506800389c2047c09eead95bc0b3224a2c0654)
Var1 Var2            t
1    10    5    1.1869810
2    50    5    0.7875174
3   100    5    1.4549489
4   500    5    5.9649529
5    10   10    0.7451685
6    50   10    1.5072754
7   100   10    3.5749858
8   500   10   12.8299009
9    10   50    7.3565341
10   50   50   22.9769044
11  100   50   64.7670850
12  500   50  271.0306328
13   10  100   19.7277996
14   50  100  113.3796667
15  100  100  272.5178511
16  500  100 1712.7072782
