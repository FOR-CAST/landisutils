# Read a LANDIS-II output raster in the row order LANDIS-II wrote it

**Always open a LANDIS-II output map with this rather than with
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).**
LANDIS-II writes its GeoTIFFs with no geotransform, which GDAL reports
as a south-up raster;
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
normalises that by reversing the rows, so the raster it returns is a
vertical mirror of the landscape LANDIS-II simulated. Nothing about the
mirrored raster looks wrong – it has the right dimensions and the right
values – so the error surfaces only as maps and per-region summaries
that disagree with the inputs.

## Usage

``` r
read_landis_raster(path, template = NULL)
```

## Arguments

- path:

  A character path to a LANDIS-II output GeoTIFF.

- template:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  or a character path to the study area rasterToMatch. When supplied,
  its CRS and extent are copied onto the result.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
whose row 1 is the first row LANDIS-II wrote, i.e. the first row of the
input maps.

## Details

This reads the file, restores the written row order when the file is
stored south-up (which is checked per file, not assumed), and optionally
georeferences the result against a template via
[`georef_landis_raster()`](https://for-cast.github.io/landisutils/reference/georef_landis_raster.md).

## See also

Other output-reading helpers:
[`georef_landis_raster()`](https://for-cast.github.io/landisutils/reference/georef_landis_raster.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sev <- read_landis_raster("fire/severity-10.tif", "rasterToMatch_Chine.tif")
} # }
```
