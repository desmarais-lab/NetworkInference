
<!-- README.md is generated from README.Rmd. Please edit that file -->
NetworkInference: Inferring Latent Diffusion Networks
=====================================================

![](https://travis-ci.org/desmarais-lab/NetworkInference.svg) ![](http://www.r-pkg.org/badges/version/NetworkInference) ![](http://cranlogs.r-pkg.org/badges/NetworkInference) ![](http://cranlogs.r-pkg.org/badges/grand-total/NetworkInference?color=yellow)

About
-----

This package provides an R implementation of the netinf algorithm created by Gomez-Rodriguez, Leskovec, and Krause (see [here](http://snap.stanford.edu/netinf/) for more information and the original C++ implementation). Given a set of events that spread between a set of nodes the algorithm infers the most likely stable diffusion network that is underlying the diffusion process.

Installation
------------

The package can be installed from [CRAN](https://CRAN.R-project.org/):

``` r
install.packages("NetworkInference")
```

The latest development version can be installed from [github](https://github.com/desmarais-lab/NetworkInference):

``` r
#install.packages(devtools)
devtools::install_github('desmarais-lab/NetworkInference')
```

Quick start guide
-----------------

This gets you started. For more in-depth explanations see the package vignette and documentation.

First, get your data into the `cascades` format required by the `netinf` function:

``` r
library(NetworkInference)

# Simulate random cascade data
set.seed(423399)
df <- simulate_rnd_cascades(50, n_node = 50) # Simulates random cascade data
node_names <- unique(df$node_name)

# Cast data into `cascades` object
## From dataframe
cascades <- as_cascade_long(df)

## From matrix
df_matrix <- as.matrix(cascades) ### Create example matrix
cascades <- as_cascade_wide(df_matrix)
```

Cascades can be explored using `summary` and `plot`. See the vignette and package documentation for full functionality.

``` r
cascade_ids <- names(cascades[[1]])
selection <- cascade_ids[c(2, 5)]
plot(cascades, label_nodes = TRUE, selection = selection)
```

<img src="readme_figures/README-unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
selection <- cascade_ids[c(1:20)]
plot(cascades, label_nodes = FALSE, selection = selection)
```

<img src="readme_figures/README-unnamed-chunk-5-2.png" style="display: block; margin: auto;" />

The actual netinf algorithm is used with:

``` r
result <- netinf(cascades, quiet = TRUE)
```

The resulting network comes in form of an edgelist:

``` r
head(result)
```

<table style="width:81%;">
<colgroup>
<col width="19%" />
<col width="26%" />
<col width="19%" />
<col width="15%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">origin_node</th>
<th align="center">destination_node</th>
<th align="center">improvement</th>
<th align="center">p_value</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">40</td>
<td align="center">22</td>
<td align="center">325.6</td>
<td align="center">8.937e-07</td>
</tr>
<tr class="even">
<td align="center">40</td>
<td align="center">25</td>
<td align="center">317.2</td>
<td align="center">2.611e-06</td>
</tr>
<tr class="odd">
<td align="center">6</td>
<td align="center">20</td>
<td align="center">316.3</td>
<td align="center">2.645e-06</td>
</tr>
<tr class="even">
<td align="center">44</td>
<td align="center">1</td>
<td align="center">315.3</td>
<td align="center">2.569e-06</td>
</tr>
<tr class="odd">
<td align="center">2</td>
<td align="center">19</td>
<td align="center">308</td>
<td align="center">2.66e-06</td>
</tr>
<tr class="even">
<td align="center">5</td>
<td align="center">13</td>
<td align="center">296.5</td>
<td align="center">7.112e-06</td>
</tr>
</tbody>
</table>

The network as well as the improvement in fit and the p-value from the vuong test per edge can be visualized:

``` r
plot(result, type = "network")
```

![](readme_figures/README-unnamed-chunk-9-1.png)

``` r
plot(result, type = "improvement")
```

![](readme_figures/README-unnamed-chunk-9-2.png)

``` r
plot(result, type = "p-value")
```

![](readme_figures/README-unnamed-chunk-9-3.png)
