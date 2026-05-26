# Find the LANDIS-II console for a local installation

Returns the path to `Landis.Console.dll` for a locally-installed
LANDIS-II. Resolution order: `LANDIS_CONSOLE` environment variable;
filesystem search under `/opt` for a `build/Release/` path.

## Usage

``` r
landis_find()
```

## Value

Character. Path to `Landis.Console.dll`, or `NA` if not found.

## See also

[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)

Other LANDIS-II execution helpers:
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
