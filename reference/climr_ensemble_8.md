# bcgov-recommended eight-member climr GCM ensemble

A character vector naming the eight GCMs that the [bcgov
ensemble-selection
guidance](https://bcgov.github.io/climr/articles/guidance_ensembleSelection.html)
identifies as a defensible subset of the full 13-model ensemble returned
by
[`climr::list_gcms()`](https://bcgov.github.io/climr/reference/data-option-lists.html).
The subset is more consistent with the IPCC assessment of climate
sensitivity than the full ensemble.

## Usage

``` r
climr_ensemble_8
```

## Value

Character vector of length 8.

## See also

[`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
[`climr::list_gcms()`](https://bcgov.github.io/climr/reference/data-option-lists.html)

## Examples

``` r
climr_ensemble_8
#> [1] "ACCESS-ESM1-5" "CNRM-ESM2-1"   "EC-Earth3"     "GFDL-ESM4"    
#> [5] "GISS-E2-1-G"   "MIROC6"        "MPI-ESM1-2-HR" "MRI-ESM2-0"   
```
