# NetworkInference 1.1.2.9000

Development version

## New Features

* The output from `netinf()` now contains information on the model, parameters and iterations as attributes. See the documentation for details.
* `netinf()` got another **speed-up**. After the first edge, the computation time for each edge is reduced by the factor number of nodes in the network
* Number of edges can now be chosen using a **Vuong style test**.
* This lead to the netinf output having a **fourth column** now, containing the 
p-value for each edge. The p-value is also available if a fixed number of edges
is chosen.
* Parameters of the diffusion model can now be chosen using approximate profile
    maximum likelihood (See [here](https://www.jstor.org/stable/2240385?seq=1#page_scan_tab_contents) and [here](https://www.jstor.org/stable/2965560?seq=1#page_scan_tab_contents) (paywalled) for more information)
* The way parameters for the diffusion distribution are chosen changed
    fundamentally. The standard behavior of `netinf()` is now the following:
    - If no starting values are provided via the `params` argument parameters
        are initialized by choosing the midpoint between the maximum possible
        parameter value and the minimum possible value. These values are derived
        using the closed form MLE of the respective parameter, derived from
        either the minimum possible diffusion times (assuming a diffusion
        'chain', i.e. `a -> b -> c -> ...`) or the maximum possible diffusion
        times (assuming a diffusion 'fan', i.e. `a -> b, a -> c, a -> d,...`).
    - Edges are inferred using those parameters until the stopping criterion
        specified in `n_edges` is satisfied
    - `n_edges` can now specify either an absolute number of edges, or a p-value
        cutoff in the interval `(0, 1)` for the Vuong test
    - New parameters are inferred from the diffusion times that result from the 
        new trees that use the inferred edges
    - The procedure is repeated until either the resulting network converges (no
        change in edges) or `max_iter` iterations are reached
* The log normal distribution is now available as a diffusion model. With this comes a **change in the parameters** for `netinf`. Instead of `lambda`, parameters are now specified with a vector (or scalar depending on distribution) `params`. For exponential and rayleigh distributions `params` is just the rate / alpha parameter. For the log-normal distribution `params` specifies mean and variance (in that order). See the `netinf()` documentation for details on specificaiton and parametrization (`?netinf`).
* If additionally `optimize=TRUE`, the parameters of the diffusion model are optimized using approximate profile maximum likelihood until the resulting network converges. 

## Bug Fixes

* Inference of very uninformative edges could lead for the software to break. Fixed now 

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
