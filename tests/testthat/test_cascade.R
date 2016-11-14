library(NetworkInference)
library(dplyr)

context("Test if cascade data structure and related methods work.")

dat <- simulate_cascades_(10)
casc <- as.cascade.data.frame(dat, node_ids = c(0:20))
df <- as.data.frame(casc)

test_that("as.cascade.data.frame works.", {
    expect_equal(length(casc), 4)
    mean_dat_times <- group_by(tbl_df(dat), cascade_id) %>%
        summarize(m_time = mean(time))
    mean_casc_times <- data_frame(cascade_id = names(casc$cascade_times),
                                  m_time = sapply(casc$cascade_times, mean))
    expect_equal(mean_dat_times, mean_casc_times)
})

test_that("as.data.frame.cascade works.", {
    dat <- dat[order(dat$cascade_id), ]
    df <- df[order(df$cascade_id), ]
    rownames(dat) <- rownames(df) <- NULL
    expect_equal(dat, df)    
})