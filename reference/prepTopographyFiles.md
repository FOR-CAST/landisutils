# Prepare Ground Slope and Uphill Azimuth raster files

- `prepGroundSlopeFile` produces a raster of percent slope;

- `prepUphillAzimuthMap` produces a raster of slope direction (degrees);

## Usage

``` r
prepGroundSlopeFile(
  studyArea = NULL,
  path = ".",
  filename = "ground_slope.tif"
)

prepUphillAzimuthMap(
  studyArea = NULL,
  path = ".",
  filename = "uphill_slope_azimuth.tif"
)
```

## Arguments

- studyArea:

  `sf` polygon, `SpatVector` or `SpatRaster` object defining the area of
  interest.

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character specifying the output filename.

## Value

Character file path.
