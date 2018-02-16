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
                                            n_edges = 10, 
                     lambda = 0.1, quiet = T), times = 50)
        t = mean(bm$time) * 1e-6
        params[i, 3] = t
}

# Current performance (4a506800389c2047c09eead95bc0b3224a2c0654)
Var1 Var2            t
1    10    5    0.4253480
2    50    5    0.8036477
3   100    5    1.3012254
4   500    5    4.6965039
5    10   10    0.7652426
6    50   10    1.3593169
7   100   10    3.3831101
8   500   10   13.6175580
9    10   50    7.2744388
10   50   50   22.9259783
11  100   50   68.5059083
12  500   50  272.2015897
13   10  100   21.0629911
14   50  100  116.0185247
15  100  100  270.7883407
16  500  100 1586.6086854
