library(NetworkInference)

context("Test if cascade data structure and related methods work.")


n_casc <- round(runif(1, 2, 676), 0)

# Data Frame methods
test_that("as.cascade.data.frame works.", {
    for(mode in c("character", "numeric", "factor")) {
        dat <- simulate_rnd_cascades(n_casc, 26, mode)
        casc <- as.cascade(data = dat, cascade_node_name = "node_name", 
                           event_time = "event_time", 
                           cascade_id = "cascade_id",
                           node_names = letters)
        df <- as.data.frame(casc)
        df <- df[order(df$cascade_id, df$node_name), ]
        dat$cascade_id <- as.character(dat$cascade_id)
        dat <- dat[order(dat$cascade_id, dat$node_name), ]
        rownames(dat) <- c(1:nrow(dat))
        rownames(df) <- c(1:nrow(df))
        expect_equal(dat, df)       
    }
})

# Matrix methods
test_that("as.matrix.cascade", {
    for(mode in c("character", "numeric", "factor")) {
        dat <- simulate_rnd_cascades(n_casc, 26, mode)
        casc <- as.cascade(data = dat, cascade_node_name = "node_name", 
                           event_time = "event_time", 
                           cascade_id = "cascade_id", 
                           node_names = letters)
        m <- as.matrix(casc)
        cascade <- as.cascade(m, node_names = letters)
        expect_equal(casc, cascade)
    }
})