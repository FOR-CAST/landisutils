# Find the LANDIS-II console path inside a Docker container

Returns the path to `Landis.Console.dll` **inside the container**. Reads
`getOption("landisutils.docker.console")`, which `.onLoad()` initializes
to the standard path used by the official LANDIS-II v8 Docker images.
Override the option in `_local.R` when using a non-standard image
layout.

## Usage

``` r
landis_find_docker()
```

## Value

Character. Path to `Landis.Console.dll` inside the container.

## See also

[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)

Other LANDIS-II execution helpers:
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
