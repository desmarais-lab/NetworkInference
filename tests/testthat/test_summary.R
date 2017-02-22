library(NetworkInference)

context("Test  summary function.")

data(cascades)

test_that("Summary runs.", {
    s <- summary(cascades)
})