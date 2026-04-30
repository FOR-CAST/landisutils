# Prepare PnET-Succession Climate input file

Writes a space-delimited PnET climate file containing monthly weather
observations (§6 of the user guide). This file is referenced from each
ecoregion (via `ClimateFileName`) when `ClimateConfigFile` is not in
use. The first line is a header; subsequent lines are monthly
observations in chronological order. Ozone (`O3`) is optional.

## Usage

``` r
prepPnETClimateFile(df = NULL, path, filename = "PnET_climate.txt")
```

## Arguments

- df:

  `data.frame` with columns `Year`, `Month`, `TMax`, `TMin`, `PAR`,
  `Prec`, `CO2`, and optionally `O3`. `Year` may be a 4-digit integer or
  a range string of the form `"1700-1979"` (see §6.3.1).

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
[`prepPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/prepPnETDisturbanceReductions.md),
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md)
