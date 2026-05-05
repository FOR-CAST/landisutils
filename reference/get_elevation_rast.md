# Get an elevation raster for a study area

Fetches an elevation raster covering `studyArea` using
[`elevatr::get_elev_raster()`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
(AWS Terrain Tiles) and converts to a `SpatRaster` in lon-lat
(`EPSG:4326`). The result seeds point locations for BioSIM via
[`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md).

## Usage

``` r
get_elevation_rast(studyArea, z = 9)
```

## Arguments

- studyArea:

  `sf` or `SpatVector` polygons object delineating the area of interest.

- z:

  integer zoom level passed to
  [`elevatr::get_elev_raster()`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html);
  higher values give finer resolution. Default `9` (~250 m at
  mid-latitudes).

## Value

`SpatRaster` of elevations in metres, in lon-lat CRS.

## See also

[`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md),
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md)
