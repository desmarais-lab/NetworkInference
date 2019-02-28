# NetworkInference 1.2.4

## New Features

## Bug Fixes
* Fixed potential memory error from iterating over the beginning/end of a `std::map`. The last fix didn't work under all circumstances.

# NetworkInference 1.2.3

## New Features

## Bug Fixes
* Fixed potential memory error from iterating over the beginning/end of a `std::map`

# NetworkInference 1.2.2

## New Features

## Bug Fixes
* `netinf()` with `log-normal` model didn't run because of an index error in the argument check for `params`
* Fixed memory allocation error caused by uninitialized comparison

# NetworkInference 1.2.1

## Bug Fixes

* `netinf_` used ceiling on integer which caused error on Solaris

# NetworkInference 1.2.0 


## New Features

#### Changes to `netinf()`

* `netinf()` got another **speed-up**. After the first edge, the computation 
    time for each edge is reduced by the factor number of nodes in the network
* Number of edges can now be chosen using a **Vuong style test**. If this 
    procedure should be used, a p-value is chosen at which the inference of new
    edges stops. This value is specified via the new `p_value_cutoff` argument
    to `netinf()`.
* This lead to the netinf output having a **fourth column** now, containing the 
p-value for each edge. The p-value is also available if a fixed number of edges
is chosen.
* If no starting values are provided via the `params` argument parameters
    are initialized by choosing the midpoint between the maximum possible
    parameter value and the minimum possible value. These values are derived
    using the closed form MLE of the respective parameter, derived from
    either the minimum possible diffusion times (assuming a diffusion
    'chain', i.e. `a -> b -> c -> ...`) or the maximum possible diffusion
    times (assuming a diffusion 'fan', i.e. `a -> b, a -> c, a -> d,...`).
* `n_edges` can now specify either an absolute number of edges, or a p-value
    cutoff in the interval `(0, 1)` for the Vuong test
* The log normal distribution is now available as a diffusion model. With this 
    comes a **change in the arguments** for `netinf`. Instead of `lambda`, 
    parameters are now specified with a vector (or scalar depending on 
    distribution) `params`. For exponential and rayleigh distributions `params` 
    is just the rate / alpha parameter. For the log-normal distribution `params` 
    specifies mean and variance (in that order). See the `netinf()` 
    documentation for details on specificaiton and parametrization (`?netinf`).
* The output from `netinf()` now contains information on the model, parameters 
    and iterations as attributes. See the documentation for details.
* The `policies` dataset has been updated with over 600 new policies from the 
    [SPID](https://doi.org/10.7910/DVN/CVYSR7) database 
    (access via `data(policies)`).
* Inferred cascade trees can now be returned by setting `trees = TRUE`.

#### New functions
* New function `drop_nodes()` now allows to drop nodes from all cascades in a cascade object.

#### Changes to `simulate_cascades()`
* `simulate_cascades()` now supports passing of additional (isolated in the diffusion network) nodes via the `nodes` argument.
* `simulate_cascades()` now also supports the log-normal distribution.


## Bug Fixes

* Inference of very uninformative edges (large number of edges) could lead for the software to break. Fixed now 
* In `simulate_cascades()` with partial cascades provided, it was possible that nodes experienced an event earlier than the last event in the partial cascade. Now, the earliest event time is the last observed event time in the partial cascade.

## Other changes

* C++ code is now modularized and headers are properly documented


# NetworkInference 1.1.2


## New Features

* We made changes to the internal data structures of the netinf function, so it is much faster and memory efficient now.
* `netinf()` now has a shiny progress bar!
* `as.cascade` is now completely removed (see release note on version 1.1.0).
* New convenience function to subset cascades by time (`subset_cascade_time`) and by cascade id (`subset_cascade`).

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
