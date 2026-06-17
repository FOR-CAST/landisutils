# Read the LANDIS-II Docker image reference used for a run

Looks for `<run_dir>/rep*/log/docker_image.log`, a one-line sidecar of
the form `ghcr.io/.../landis-ii-v8-release@sha256:<64hex>` written by
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
after `docker pull`. The immutable `sha256` digest pins the exact
runtime image; an image tag alone is mutable.

## Usage

``` r
landis_image_info(run_dir)
```

## Arguments

- run_dir:

  Character path to a scenario run directory (containing `rep*`
  subdirectories).

## Value

A named list with `image_name`, `image_digest`, and `source`:

- `"captured"`:

  Sidecar present (authoritative; the digest pins the runtime).

- `"option"`:

  Sidecar absent; image name read from the `landisutils.docker.image`
  option on the current host (best-effort; the tag is mutable).

- `"local"`:

  No sidecar and no Docker option set (a local install).

## See also

Other provenance helpers:
[`parse_landis_log_versions()`](https://for-cast.github.io/landisutils/reference/parse_landis_log_versions.md),
[`prov_landis_container()`](https://for-cast.github.io/landisutils/reference/prov_landis_container.md),
[`prov_landis_versions()`](https://for-cast.github.io/landisutils/reference/prov_landis_versions.md),
[`prov_run_resources()`](https://for-cast.github.io/landisutils/reference/prov_run_resources.md),
[`prov_stochasticity()`](https://for-cast.github.io/landisutils/reference/prov_stochasticity.md)
