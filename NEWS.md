# NetworkInference 1.1.1.9000

Current development version

# New Features

* `as.cascade` is now completely removed (see release note on version 1.1.0).
* The `node_names` argument for `as_cascade_long` and `as_cascade_wide` will be
phased out since it is not doing much. It is still available but not recommended
anymore.
* New convenience function to subset cascades by time (`subset_cascade_time`) and by cascade id (`subset_cascade`).
* We made changes to the internal data structures of the netinf function, so it is much faster and memory efficient now.
* `netinf()` now has a shiny progress bar!

## Bug Fixes
* Long running functions (that call compiled code) can now be interrupted without crashing the R session.
* `as_cascade_long()` and `as_cascade_wide()` handle date input correctly now.
* `as_cascade_wide()` couldn't handle data input of class `data.table`. 


# NetworkInference 1.1.1

## Bug Fixes

* Use of igraph now conditional compliant with Writing R Extensions [1.1.3.1](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Suggested-packages)
* Fixed version number displayed in startup message


# NetworkInference 1.1.0

## New Features

* Data format (long or wide) of `as.cascade` is not bound to the class of the data object anymore. In 1.0.0 wide format had to be a matrix and long format had to be a dataframe. This did not make much sense. `as.cascade` is now deprecated and replaced by two new functions `as_cascade_long` and `as_cascade_wide`.

## Bug Fixes

* x and y axis labels in `plot.cascade` with option `label_nodes=FALSE` were
    reversed


# NetworkInference 1.0.0

First release