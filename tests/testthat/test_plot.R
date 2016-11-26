library(NetworkInference)

context("Test if plotting works.")

data(cascades)

test_that("Plotting function runs.", {
    p <- plot(cascades, selection = names(cascades$cascade_nodes)[1:3])
    p <- plot(cascades, label_nodes = FALSE)
    expect_warning(plot(cascades))
})