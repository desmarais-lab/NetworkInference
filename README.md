
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NetworkInference: Inferring Latent Diffusion Networks

![](https://travis-ci.org/desmarais-lab/NetworkInference.svg)
![](http://www.r-pkg.org/badges/version/NetworkInference)
![](http://cranlogs.r-pkg.org/badges/NetworkInference)
![](http://cranlogs.r-pkg.org/badges/grand-total/NetworkInference?color=yellow)

## About

This package provides an R implementation of the netinf algorithm
created by Gomez-Rodriguez, Leskovec, and Krause (see
[here](http://snap.stanford.edu/netinf/) for more information and the
original C++ implementation). Given a set of events that spread between
a set of nodes the algorithm infers the most likely stable diffusion
network that is underlying the diffusion process.

## Installation

The package can be installed from [CRAN](https://CRAN.R-project.org/):

``` r
install.packages("NetworkInference")
```

The latest development version can be installed from
[github](https://github.com/desmarais-lab/NetworkInference):

``` r
#install.packages(devtools)
devtools::install_github('desmarais-lab/NetworkInference')
```

## Quick start guide

To get started, get your data into the `cascades` format required by the
`netinf` function:

``` r
library(NetworkInference)

# Simulate random cascade data
df <- simulate_rnd_cascades(50, n_node = 20)

# Cast data into `cascades` object
## From long format
cascades <- as_cascade_long(df)

## From wide format
df_matrix <- as.matrix(cascades) ### Create example matrix
cascades <- as_cascade_wide(df_matrix)
```

Then fit the model:

``` r
result <- netinf(cascades, quiet = TRUE, p_value_cutoff = 0.05)
```

``` r
head(result)
```

| origin\_node | destination\_node | improvement | p\_value  |
| :----------: | :---------------: | :---------: | :-------: |
|      20      |         7         |    290.1    | 7.324e-06 |
|      8       |        17         |     272     | 1.875e-05 |
|      3       |         2         |    270.5    | 1.87e-05  |
|      20      |         5         |    262.8    | 1.899e-05 |
|      7       |        16         |    250.4    | 4.779e-05 |
|      20      |        15         |     249     | 4.774e-05 |
