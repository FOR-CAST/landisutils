# landisutils

<!-- badges: start -->
[![R-CMD-check](https://github.com/FOR-CAST/landisutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FOR-CAST/landisutils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Tools and utilities for preparing data for and running LANDIS-II simulations, and post-processing their outputs.

## Installation

You can install the development version of `landisutils` like so:

``` r
remotes::install_github("FOR-CAST/landisutils")
```

## Preparing data

Use `prep*()` functions to convert input data to LANDIS-II data formats.

```r
ic_files <- prepInitialCommunities()
```

## Creating LANDIS-II input files

Use `insert*()` functions when generating LANDIS-II input text files.

``` r
insertInitialCommunities(ic_files) |>
  writeLines() ## TODO
```

## Creating LANDIS-II scenario files

Use `scenario()` to construct scenario files.

```r
pix_size <- 250 ## TODO: get this from the prepared data
sim_time <- 200

scenario(
  cell_length = pix_size,
  duration = sim_time,
  extensions = list(
    succession = list(),
    disturbance = list(),
    other = list()
  ),
  name = "",
  path = file.path(),
  version = 7
)
## TODO - deal with more detailed customizations later
```
