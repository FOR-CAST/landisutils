# Build batched point-location table from a study area

Extracts the lon-lat / elevation of every cell in `elev` that overlaps
`studyArea`, tags each cell with its containing polygon's `id` value
(column `EcoID`) and a numeric `BatchID`, and splits into a list of
`batch_size`-row data frames suitable for chunked dispatch through
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md).

## Usage

``` r
create_locations_df(elev, studyArea, id, batch_size = 1000)
```

## Arguments

- elev:

  `SpatRaster` of elevations in lon-lat CRS (typically from
  [`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md)).

- studyArea:

  `sf` or `SpatVector` polygons object delineating the area of interest.

- id:

  character. Name of the field in `studyArea` identifying ecoregions
  (e.g. `"PolyID"`).

- batch_size:

  integer. Maximum cells per batch. Default `1000`.

## Value

A list of data frames with columns `ID`, `Longitude`, `Latitude`,
`Elevation`, `EcoID`, `BatchID` (one element per batch).

## See also

[`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md),
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md)
