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
