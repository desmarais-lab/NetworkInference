
# NetworkInference 1.1.0

## New Features

* Data format (long or wide) of `as.cascade` is not bound to the class of the data object anymore. In 1.0.0 wide format had to be a matrix and long format had to be a dataframe. This did not make much sense. `as.cascade` is now deprecated and replaced by two new functions `as_cascade_long` and `as_cascade_wide`.

## Bug Fixes

* x and y axis labels in `plot.cascade` with option `label_nodes=FALSE` were
    reversed

# NetworkInference 1.0.0

First release
