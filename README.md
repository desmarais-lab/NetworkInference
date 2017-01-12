[![Travis-CI Build Status](https://travis-ci.org/flinder/NetworkInference.svg?branch=master)](https://travis-ci.org/flinder/NetworkInference)

# NetworkInference

## About

R implementation of the [netinf](http://snap.stanford.edu/netinf/) algorithm.

## Use
```R
devtools::install_github('flinder/NetworkInference')
library(NetworkInference)

data(cascades)
out <- netinf(cascades, trans_mod = "exponential", n_edges = 5, lambda = 1)
```