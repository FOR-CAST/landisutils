# Specify Scenario Extensions Tables

A scenario must specify exactly one succession extension, zero or more
disturbance extensions, and zero or more other extensions.

## Usage

``` r
insertSuccessionExtensions(exts = NULL)

insertDisturbanceExtensions(exts = NULL)

insertOtherExtensions(exts = NULL)
```

## Arguments

- exts:

  Named list of extensions specifying the initialization file to use.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## Examples

``` r
list("Biomass Succession" = "biomass-succession.txt") |>
  insertSuccessionExtensions()
#> [1] ">> Succession Extension    Initialization File"         
#> [2] ">> --------------------    -------------------"         
#> [3] "   \"Biomass Succession\"        biomass-succession.txt"
#> [4] ""                                                       

list(
  "Base Fire" = "base-fire.txt",
  "Base Harvest" = "base-harvest.txt"
) |>
  insertDisturbanceExtensions()
#> [1] ">> Disturbance Extensions    Initialization File"
#> [2] ">> ----------------------    -------------------"
#> [3] "   \"Base Fire\"        base-fire.txt"           
#> [4] "   \"Base Harvest\"        base-harvest.txt"     
#> [5] ""                                                

list(
  "Output Biomass By Age" = "output-biomass-by-age.txt",
  "Output Cohort Statistics" = "output-cohort-statistics.txt"
) |>
  insertOtherExtensions()
#> [1] ">> Other Extensions            Initialization File"                 
#> [2] ">> ------------------------    -----------------------"             
#> [3] "   \"Output Biomass By Age\"        output-biomass-by-age.txt"      
#> [4] "   \"Output Cohort Statistics\"        output-cohort-statistics.txt"
#> [5] ""                                                                   
```
