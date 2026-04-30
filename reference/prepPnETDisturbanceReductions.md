# Prepare PnET-Succession Disturbance Reductions file

Writes the disturbance reductions file (§10 of the user guide), which
specifies how each disturbance partitions biomass into / out of the dead
pools. Rows correspond to the five reductions in
[.pnetDisturbancePools](https://for-cast.github.io/landisutils/reference/dot-pnetDisturbancePools.md);
remaining columns are the disturbance types active in the scenario (e.g.
`fire`, `wind`, `harvest`, `bda`).

## Usage

``` r
prepPnETDisturbanceReductions(
  df = NULL,
  path,
  filename = "DisturbanceReductions.txt"
)
```

## Arguments

- df:

  `data.frame` whose first column is `Pool` (with values in
  [.pnetDisturbancePools](https://for-cast.github.io/landisutils/reference/dot-pnetDisturbancePools.md))
  and whose remaining columns are disturbance types. Use
  [`defaultPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/defaultPnETDisturbanceReductions.md)
  for a starting point. Reduction values must be in `[0, 1]`.

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
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md)
