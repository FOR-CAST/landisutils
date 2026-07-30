# Pick a raster pixel type LANDIS-II can read

Returns the smallest terra `datatype` that LANDIS-II will open and that
can hold `max_value`. Use it for any integer-coded map written for
LANDIS-II (initial communities, ecoregions, fire regions), rather than
choosing a datatype by hand.

## Usage

``` r
landis_datatype(max_value)
```

## Arguments

- max_value:

  Largest value the map has to represent. Must be finite and
  non-negative.

## Value

A length-1 character `datatype` suitable for
[`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html):
`"INT1U"`, `"INT2S"` or `"INT4S"`.

## Details

LANDIS-II opens maps through
`Landis.RasterIO.Gdal.GdalInputRaster.NewInputBand`, which accepts only
GDAL `Byte`, `Int16`, `Int32`, `Float32` and `Float64`, and aborts the
run with `Raster band is not byte, short, int, float, double` on
anything else. The unsigned integer types are therefore unusable even
though map codes are always positive – as is `INT1S`, which GDAL 3.7+
writes as `Int8`. Because the check happens when the extension
initialises rather than when the file is written, an unreadable map is
only discovered by running the model.

## Examples

``` r
landis_datatype(200) ## "INT1U"
#> [1] "INT1U"
landis_datatype(5000) ## "INT2S" -- NOT "INT2U", which LANDIS-II rejects
#> [1] "INT2S"
landis_datatype(1e6) ## "INT4S"
#> [1] "INT4S"
```
