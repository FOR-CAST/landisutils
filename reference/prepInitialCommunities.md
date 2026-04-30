# Create `InitialCommunities` and `InitialCommunitiesMap` Files

Create `InitialCommunities` and `InitialCommunitiesMap` Files

## Usage

``` r
prepInitialCommunities(cohortData, pixelGroupMap, path)
```

## Arguments

- cohortData:

  A `data.table` containing cohort information (see LandR)

- pixelGroupMap:

  A `SpatRaster` identifying the locations of the pixel groups in
  `cohortData`

- path:

  Character. Path specifying a directory to use for the scenario runs.

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## See also

Used by succession extensions:
[BiomassSuccession](https://for-cast.github.io/landisutils/reference/BiomassSuccession.md),
[DGSSuccession](https://for-cast.github.io/landisutils/reference/DGSSuccession.md),
[ForCS](https://for-cast.github.io/landisutils/reference/ForCS.md),
[NECNSuccession](https://for-cast.github.io/landisutils/reference/NECNSuccession.md),
and
[PnETSuccession](https://for-cast.github.io/landisutils/reference/PnETSuccession.md).
