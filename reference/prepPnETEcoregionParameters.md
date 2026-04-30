# Prepare PnET-Succession Ecoregion Parameters file

Writes the PnET ecoregion parameters file (§9 of the user guide). One
row per active ecoregion. Required columns are listed in
[.pnetEcoregionRequiredCols](https://for-cast.github.io/landisutils/reference/dot-pnetEcoregionRequiredCols.md);
allowed optional columns are listed in
[.pnetEcoregionOptionalCols](https://for-cast.github.io/landisutils/reference/dot-pnetEcoregionOptionalCols.md).
`ClimateFileName` is required when the top-level `ClimateConfigFile` is
not in use.

## Usage

``` r
prepPnETEcoregionParameters(
  df = NULL,
  path,
  filename = "EcoregionParameters.txt"
)
```

## Arguments

- df:

  `data.frame` with one row per ecoregion. The first column must be
  `EcoregionName` (matching the ecoregion names in the LANDIS-II
  ecoregion input file).

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## See also

Other PnET Succession helpers:
[`PnETSuccession`](https://for-cast.github.io/landisutils/reference/PnETSuccession.md),
[`defaultPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/defaultPnETDisturbanceReductions.md),
[`prepPNEToutputsites()`](https://for-cast.github.io/landisutils/reference/prepPNEToutputsites.md),
[`prepPnETClimateFile()`](https://for-cast.github.io/landisutils/reference/prepPnETClimateFile.md),
[`prepPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/prepPnETDisturbanceReductions.md),
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md)
