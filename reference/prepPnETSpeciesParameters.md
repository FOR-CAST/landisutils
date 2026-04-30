# Prepare PnET-Succession Species Parameters file

Writes the PnET species parameters file (§8 of the user guide). One row
per species. Required columns are listed in
[.pnetSpeciesRequiredCols](https://for-cast.github.io/landisutils/reference/dot-pnetSpeciesRequiredCols.md);
allowed optional columns are listed in
[.pnetSpeciesOptionalCols](https://for-cast.github.io/landisutils/reference/dot-pnetSpeciesOptionalCols.md).
Unknown columns trigger an error.

## Usage

``` r
prepPnETSpeciesParameters(
  df = NULL,
  path,
  filename = "PnETSpeciesParameters.txt"
)
```

## Arguments

- df:

  `data.frame` with one row per species. The first column must be
  `SpeciesCode` (species name as it appears in the LANDIS-II species
  file).

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
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md)
