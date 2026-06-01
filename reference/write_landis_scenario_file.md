# Write a LANDIS-II `scenario.txt` file (lower-level helper)

Generates a `scenario.txt` in `path` using the `insert*()` helpers.
Lower-level than
[`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md):
takes already-written extension config-file paths (named character
vectors keyed by extension name) rather than R6
[LandisExtension](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
objects. Useful when project pipelines write extension configs in
separate `targets`-managed steps and just want to assemble the top-level
scenario file from those paths.

## Usage

``` r
write_landis_scenario_file(
  path,
  duration,
  cell_length,
  species_file,
  ecoregions_files,
  succession_ext_files,
  disturbance_ext_files = NULL,
  other_ext_files = NULL,
  output_manifest = character(0)
)
```

## Arguments

- path:

  Character. Scenario directory path (absolute or relative).

- duration:

  Integer. Simulation duration in years.

- cell_length:

  Integer. Raster cell size in metres.

- species_file:

  Character. Path to the core species input file.

- ecoregions_files:

  Character vector of length 2: paths to the ecoregions text file and
  the ecoregions raster map.

- succession_ext_files:

  Named character vector. Names are extension names (e.g.
  `"ForC Succession"`); values are absolute paths to the per-extension
  init files. Exactly one entry is required.

- disturbance_ext_files, other_ext_files:

  Named character vector or `NULL`. Same structure as
  `succession_ext_files`; zero or more entries.

- output_manifest:

  Character vector of fixed-name output files (paths relative to each
  replicate directory) that the scenario will produce at run time.
  Written to `output_manifest.txt` so
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  can track these files alongside the recursive `output_dir` scan.

## Value

Character scalar: absolute path to the written `scenario.txt`.

## Details

The `RandomNumberSeed` line is written in commented-out form so that
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)
can overwrite it with a per-replicate deterministic seed (controlled by
its `base_seed` argument).

## See also

[`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
