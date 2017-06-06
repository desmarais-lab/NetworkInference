library(NetworkInference)

set.seed(2552)
context("Test if cascade data structure and related methods work.")

for(n_casc in c(1,10)) {
    # Long format
    msg <- paste("as.cascade.data.frame, as.data.frame.cascade work with", 
                  n_casc, "cascade(s).")
    test_that(msg, {
        for(mode in c("character", "numeric", "factor")) {
            dat <- simulate_rnd_cascades(n_casc, 26, mode)
            casc <- as_cascade_long(data = dat, cascade_node_name = "node_name", 
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
    
    # Wide format
    msg <- paste("as.cascade.matrix, as.matrix.cascade work with", 
                  n_casc, "cascade(s).")
    test_that(msg, {
        for(mode in c("character", "numeric", "factor")) {
            dat <- simulate_rnd_cascades(n_casc, 26, mode)
            casc <- as_cascade_long(data = dat, cascade_node_name = "node_name", 
                                    event_time = "event_time", 
                                    cascade_id = "cascade_id", 
                                    node_names = letters)
            m <- as.matrix(casc)
            cascade <- as_cascade_wide(m, node_names = letters)
            expect_equal(casc, cascade)
        }
    })
}
