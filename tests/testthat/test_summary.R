library(NetworkInference)

context("Test  summary function.")

data(cascades)

test_that("Summary runs.", {
    x <- capture.output(s <- summary(cascades))
})