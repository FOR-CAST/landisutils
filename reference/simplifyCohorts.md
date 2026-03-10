# Simplify cohorts

Reduce the number of cohorts / pixel groups for LANDIS-II, which only
supports (integer) initial community map codes between 0 and 65535.

## Usage

``` r
simplifyCohorts(cohortData, pixelGroupMap, ageBin = 20)
```

## Arguments

- cohortData:

  A `data.table` containing cohort information (see LandR)

- pixelGroupMap:

  A `SpatRaster` identifying the locations of the pixel groups in
  `cohortData`

- ageBin:

  integer specifying the bin width for the new age categories

## Value

list containing updated `cohortData` and `pixelGroupMap` objects

## Note

Ideally, the user should reduce the number of cohorts upstream (i.e., in
`Biomass_borealDataPrep`), to ensure consistency of all data inputs.
