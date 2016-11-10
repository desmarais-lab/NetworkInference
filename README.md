[![Travis-CI Build Status](https://travis-ci.org/flinder/NetworkInference.svg?branch=master)](https://travis-ci.org/flinder/NetworkInference)

# NetworkInference

R interface to the stanford network analysis project's (SNAP) netinf algorithm.


```R
devtools::install_github('flinder/NetworkInference')
library(NetworkInference)

data(example_cascades)
out <- netinf(node_ids = example_cascades$node_ids, 
              node_names = example_cascades$node_names, 
              cascade_ids = example_cascades$cascade_ids, 
              cascade_times = example_cascades$cascade_times,
              trans_mod = "exponential", alpha = 1, verbose = TRUE)
```