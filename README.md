[![Travis-CI Build Status](https://travis-ci.org/flinder/NetworkInference.svg?branch=master)](https://travis-ci.org/flinder/NetworkInference)

# NetworkInference

R interface to the stanford network analysis project's (SNAP) netinf algorithm.


```R
devtools::install_github('flinder/NetworkInference')
library(NetworkInference)

data(cascades)
out <- netinf(cascades, trans_mod = "exponential", alpha = 1, verbose = TRUE)
```