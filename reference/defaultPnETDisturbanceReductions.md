# Default PnET-Succession `DisturbanceReductions` table

Returns a sensible default
[data.frame](https://rdrr.io/r/base/data.frame.html) for the
PnET-Succession `DisturbanceReductions` input file with columns for the
four most common disturbance types (`fire`, `wind`, `harvest`, `bda`).
Values are taken from the example in §10.1 of the user guide and should
be tuned to the study system. The first column (`Pool`) names the
dead-pool reduction, drawn from
[.pnetDisturbancePools](https://for-cast.github.io/landisutils/reference/dot-pnetDisturbancePools.md).

## Usage

``` r
defaultPnETDisturbanceReductions()
```

## Value

`data.frame`

## See also

Other PnET Succession helpers:
[`PnETSuccession`](https://for-cast.github.io/landisutils/reference/PnETSuccession.md),
[`prepPNEToutputsites()`](https://for-cast.github.io/landisutils/reference/prepPNEToutputsites.md),
[`prepPnETClimateFile()`](https://for-cast.github.io/landisutils/reference/prepPnETClimateFile.md),
[`prepPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/prepPnETDisturbanceReductions.md),
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md)
