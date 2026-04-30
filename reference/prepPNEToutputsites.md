# Prepare PnET-Succession `PNEToutputsites` input file

Writes the `PNEToutputsites` input file (§12 of the user guide), which
lists individual sites at which detailed PnET output tables are
requested. Two coordinate forms are supported:

## Usage

``` r
prepPNEToutputsites(
  df = NULL,
  path,
  coords = c("rowcol", "map"),
  filename = "PNEToutputsites.txt"
)
```

## Arguments

- df:

  `data.frame` whose first column (`Site`) names each site, with the
  remaining columns matching `coords`.

- path:

  Character. Path specifying a directory to use for the scenario runs.

- coords:

  Character, one of `"map"` or `"rowcol"`. Default `"rowcol"`.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## Details

- **map coordinates** (`coords = "map"`): four numeric columns
  `MapCoordinatesX`, `MapCoordinatesY`, `MapCoordinatesMaxX`,
  `MapCoordinatesMaxY`.

- **row/column coordinates** (`coords = "rowcol"`): two integer columns
  `Row`, `Column`.

## See also

Other PnET Succession helpers:
[`PnETSuccession`](https://for-cast.github.io/landisutils/reference/PnETSuccession.md),
[`defaultPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/defaultPnETDisturbanceReductions.md),
[`prepPnETClimateFile()`](https://for-cast.github.io/landisutils/reference/prepPnETClimateFile.md),
[`prepPnETDisturbanceReductions()`](https://for-cast.github.io/landisutils/reference/prepPnETDisturbanceReductions.md),
[`prepPnETEcoregionParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETEcoregionParameters.md),
[`prepPnETGenericParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETGenericParameters.md),
[`prepPnETSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/prepPnETSpeciesParameters.md)
