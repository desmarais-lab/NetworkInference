library(NetworkInference)

context("Test if core netinf method works")

data(cascades)
data(validation)
test_that("netinf produces the edges as original netinf executable.", {
    from_netinf <- netinf(cascades, lambda = 1, trans_mod = "exponential",
                          n_edges = 5)
    t1 <- from_netinf[order(from_netinf[, 1], from_netinf[, 2]), -3]
    rownames(t1) <- c(1:nrow(t1))
    t2 <- validation[order(validation[, 1], validation[, 2]), -c(3:6)]
    rownames(t2) <- c(1:nrow(t2))
    class(t2) <- c("diffnet", "data.frame")
    expect_equal(t1, t2)
})

test_that("netinf scales.", {
    
    out <- lapply(seq(10, 200, 50), function(n_nodes) {
        print(n_nodes)
        X <- lapply(seq(5, 50, 10), function(n_cascades) {
            print(n_cascades)
            s <- Sys.time()
            cascades <- as_cascade_long(simulate_rnd_cascades(n_cascades, n_nodes))
            t_data <- as.numeric(Sys.time() - s)
            s <- Sys.time()
            netinf(cascades, lambda=1, trans_mod="exponential", n_edges=10) 
            t_netinf <- as.numeric(Sys.time() - s) 
            return(c(n_nodes, n_cascades, t_data, t_netinf)) 
        })
        return(do.call(rbind, X))
    }) 
    
    library(tidyverse)
    library(reshape2)
    df <- as_data_frame(do.call(rbind, out))    
    colnames(df) <- c("n_nodes", "n_cascades", "t_data", "t_netinf")
     
    # Estimation time as function of nodes 
    ggplot(df, aes(x=n_nodes, y=n_cascades)) +
        #geom_raster(aes(fill = t_netinf)) +
        #scale_fill_gradient2(low="navy", mid="white", high="red", 
        #                 midpoint=0, limits=range(df$t_netinf)) +
        geom_point(aes(size=t_netinf)) +
        theme_bw()
    
    ggplot(df, aes(x = n_nodes, y = t_netinf)) +
        geom_point(aes(size = n_cascades)) +
        stat_summary(fun.data = "mean_cl_normal", color="red")
    
    ggplot(group_by(df, n_cascades) %>% summarize(mean_time_over_n_nodes=mean(t_netinf))) +
        geom_line(aes(x = n_cascades, y = mean_time_over_n_nodes))
    ggplot(group_by(df, n_nodes) %>% summarize(mean_time_over_n_cascades=mean(t_netinf))) +
        geom_line(aes(x = n_nodes, y = mean_time_over_n_cascades))
    
    
    library(microbenchmark)
    n_cascades = 100
    n_nodes = 100
    cascades <- as_cascade_long(simulate_rnd_cascades(n_cascades, n_nodes))
    nodes = lapply(cascades$cascade_nodes, as.integer)
    times = cascades$cascade_times
    microbenchmark(count_possible_edges_(nodes, times), times = 20)
    
    }

})