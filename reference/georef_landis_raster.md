# Attach CRS and extent from a template to a LANDIS-II raster

LANDIS-II GeoTIFFs are written without a spatial reference (a raw
row/col grid). This copies the coordinate reference system and extent
from a template raster (typically the study area's rasterToMatch) onto a
LANDIS-II output of identical dimensions, making it analysis-ready.

## Usage

``` r
georef_landis_raster(r, template)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  or a character path to a LANDIS-II GeoTIFF.

- template:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  or a character path to the study area rasterToMatch (defines the CRS
  and extent to copy).

## Value

The georeferenced
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Details

This stamps georeferencing on; it does not resample or reproject. It
DOES reorient, but only when it can see the file the raster came from:
given a path, or a `SpatRaster` still backed by one, the row order is
restored via
[`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md).
A `SpatRaster` already loaded into memory carries no record of how it
was read, so orientation cannot be checked – read LANDIS-II outputs with
[`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md)
and derive from there.

## See also

Other output-reading helpers:
[`read_landis_raster()`](https://for-cast.github.io/landisutils/reference/read_landis_raster.md)
