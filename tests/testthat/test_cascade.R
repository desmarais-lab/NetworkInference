library(NetworkInference)

context("Test if cascade data structure and related methods work.")


test_as.cacade.data.frame_times <- function(dat, casc) {
    mean_casc_times <- sapply(casc$cascade_times, mean)
    mean_dat_times <- sapply(names(mean_casc_times), 
                             function(x) mean(dat[dat[, 3] == x, 2]))
    return(list(mean_casc_times, mean_dat_times))
}

n_casc <- round(runif(1, 2, 676), 0)

test_that("as.cascade.data.frame works with numeric cascade ids.", {
    dat <- simulate_cascades_(n_casc, "numeric")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])
    el_per_casc_id <- sapply(casc$cascade_nodes, length)
    el_per_casc_time <- sapply(casc$cascade_times, length)
    out <- test_as.cacade.data.frame_times(dat, casc) 
    
    expect_equal(length(casc), 3)
    expect_equal(length(casc$cascade_nodes), n_casc)
    expect_equal(length(casc$cascade_times), n_casc)
    expect_equal(el_per_casc_id, el_per_casc_time)
    expect_equal(out[[1]], out[[2]])
})

test_that("as.cascade.data.frame works with factor cascade ids.", {
    dat <- simulate_cascades_(n_casc, "factor")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])
    el_per_casc_id <- sapply(casc$cascade_nodes, length)
    el_per_casc_time <- sapply(casc$cascade_times, length)
    out <- test_as.cacade.data.frame_times(dat, casc) 
    
    expect_equal(length(casc), 3)
    expect_equal(length(casc$cascade_nodes), n_casc)
    expect_equal(length(casc$cascade_times), n_casc)
    expect_equal(el_per_casc_id, el_per_casc_time)
    expect_equal(out[[1]], out[[2]])
})

test_that("as.cascade.data.frame works with character cascade ids.", {
    dat <- simulate_cascades_(n_casc, "character")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])
    el_per_casc_id <- sapply(casc$cascade_nodes, length)
    el_per_casc_time <- sapply(casc$cascade_times, length)
    out <- test_as.cacade.data.frame_times(dat, casc) 
    
    expect_equal(length(casc), 3)
    expect_equal(length(casc$cascade_nodes), n_casc)
    expect_equal(length(casc$cascade_times), n_casc)
    expect_equal(el_per_casc_id, el_per_casc_time)
    expect_equal(out[[1]], out[[2]])
})

test_that("as.data.frame.cascade works with numeric cascade ids.", {
    dat <- simulate_cascades_(n_casc, "numeric")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])   
    df <- as.data.frame(casc)
    # cacade_id is transformed to character in as.cascade. Transform back to 
    # allow comparision
    df[, 3] <- as.numeric(df[, 3])
    dat <- dat[order(dat$cascade_id), ]
    df <- df[order(df$cascade_id), ]
    rownames(dat) <- rownames(df) <- NULL
    expect_equal(dat, df)    
})

test_that("as.data.frame.cascade works with factor cascade ids.", {
    dat <- simulate_cascades_(n_casc, "factor")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])   
    df <- as.data.frame(casc)
    # cacade_id is transformed to character in as.cascade. Transform back to 
    # allow comparision
    df[, 3] <- as.factor(df[, 3])
    dat <- dat[order(dat$cascade_id), ]
    df <- df[order(df$cascade_id), ]
    rownames(dat) <- rownames(df) <- NULL
    expect_equal(dat, df)    
})

test_that("as.data.frame.cascade works with character cascade ids.", {
    dat <- simulate_cascades_(n_casc, "character")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])   
    df <- as.data.frame(casc)
    dat <- dat[order(dat$cascade_id), ]
    df <- df[order(df$cascade_id), ]
    rownames(dat) <- rownames(df) <- NULL
    expect_equal(dat, df)    
})

test_that("as.matrix.cascade character", {
    dat <- simulate_cascades_(n_casc, "character")
    casc <- as.cascade(dat, node_names = letters[1:20])
    m <- as.matrix(casc)
    expect_equal(dim(m), c(20, n_casc))
    expect_equal(rownames(m), casc$node_names)
    expect_equal(colnames(m), names(casc$cascade_times))
})

test_that("as.cascade.matrix works with numeric cascade ids.", {
    dat <- simulate_cascades_(n_casc, "numeric")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])
    casc <- as.cascade(as.matrix(casc))
    el_per_casc_id <- sapply(casc$cascade_nodes, length)
    el_per_casc_time <- sapply(casc$cascade_times, length)
    out <- test_as.cacade.data.frame_times(dat, casc) 
    
    expect_equal(length(casc), 3)
    expect_equal(length(casc$cascade_nodes), n_casc)
    expect_equal(length(casc$cascade_times), n_casc)
    expect_equal(el_per_casc_id, el_per_casc_time)
    expect_equal(out[[1]], out[[2]])
})

test_that("as.cascade.data.frame works with factor cascade ids.", {
    dat <- simulate_cascades_(n_casc, "factor")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])
    casc <- as.cascade(as.matrix(casc))
    el_per_casc_id <- sapply(casc$cascade_nodes, length)
    el_per_casc_time <- sapply(casc$cascade_times, length)
    out <- test_as.cacade.data.frame_times(dat, casc) 
    
    expect_equal(length(casc), 3)
    expect_equal(length(casc$cascade_nodes), n_casc)
    expect_equal(length(casc$cascade_times), n_casc)
    expect_equal(el_per_casc_id, el_per_casc_time)
    expect_equal(out[[1]], out[[2]])
})

test_that("as.cascade.data.frame works with character cascade ids.", {
    dat <- simulate_cascades_(n_casc, "character")
    casc <- as.cascade.data.frame(cascade_node_name = "node_name", 
                                  event_time = "event_time", 
                                  cascade_id = "cascade_id", data = dat, 
                                  node_names = letters[0:20])
    casc <- as.cascade(as.matrix(casc))
    el_per_casc_id <- sapply(casc$cascade_nodes, length)
    el_per_casc_time <- sapply(casc$cascade_times, length)
    out <- test_as.cacade.data.frame_times(dat, casc) 
    
    expect_equal(length(casc), 3)
    expect_equal(length(casc$cascade_nodes), n_casc)
    expect_equal(length(casc$cascade_times), n_casc)
    expect_equal(el_per_casc_id, el_per_casc_time)
    expect_equal(out[[1]], out[[2]])
})
