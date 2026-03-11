# Prepare topographical raster files

- `prepGroundSlopeFile` produces a raster of slope (degrees);

- `prepUphillAzimuthMap` produces a raster of slope direction (degrees);

## Usage

``` r
prepTopographyFile(aoi, type, path = ".", filename = NULL)

prepGroundSlopeFile(aoi = NULL, path = ".", filename = "ground_slope.tif")

prepUphillAzimuthMap(
  aoi = NULL,
  path = ".",
  filename = "uphill_slope_azimuth.tif"
)
```

## Arguments

- aoi:

  `sf` polygon, `SpatVector` or `SpatRaster` object defining the area of
  interest.

- type:

  Character. One of the allowed types for `v` in
  [terra::terrain](https://rspatial.github.io/terra/reference/terrain.html),
  e.g., "slope" or "aspect".

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character specifying the output filename.

## Value

Character file path.
