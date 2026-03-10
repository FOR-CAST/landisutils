# Find an installed version of LANDIS-II

Find an installed version of LANDIS-II

## Usage

``` r
landis_find()
```

## Value

Character vector. File path(s), if found, to any LANDIS-II executables.

To ensure this function can find your LANDIS-II installation, please
ensure that the `LANDIS_CONSOLE` environment variable is set and
accessible by in R (e.g., `Sys.getenv("LANDIS_CONSOLE")` should return a
non-empty value).
