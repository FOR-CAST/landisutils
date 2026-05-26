# Create replicate directories for a LANDIS-II scenario

Creates `n_reps` numbered replicate sub-directories inside
`scenario_dir` (`rep01/`, `rep02/`, …) and copies input files into each.
If `files` is supplied it is used as the explicit copy list; GDAL
sidecar files (`.tfw`, `.aux.xml`) that accompany any `.tif` in the list
are included automatically. When `files` is `NULL` the fallback is every
top-level file in `scenario_dir` (sub-directories are never copied). The
function is idempotent: existing replicate directories are left
untouched, so adding more replicates later never alters
previously-created ones.

## Usage

``` r
landis_replicate(scenario_dir, n_reps, files = NULL, base_seed = NULL)
```

## Arguments

- scenario_dir:

  Character. Path to the base scenario directory.

- n_reps:

  Integer. Number of replicates to create.

- files:

  Character vector or `NULL`. Explicit set of file paths to copy into
  each replicate directory. Typically the values of the `deps` targets
  passed to
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  (already absolute paths). When `NULL`, all top-level files in
  `scenario_dir` are copied.

- base_seed:

  Integer or `NULL`. When non-`NULL`, the `RandomNumberSeed` in each
  copied `scenario.txt` is set to `base_seed + (rep_index - 1)`. Rep
  indices are 1-based (`rep01` → `base_seed`, `rep02` → `base_seed + 1`,
  …).

## Value

Character vector of absolute paths to the replicate directories, in
replicate order.

## Details

When `base_seed` is provided the `RandomNumberSeed` line in each rep's
`scenario.txt` is rewritten to `base_seed + (rep_index - 1)`, giving
every replicate a distinct but deterministic seed. Because seeds are
derived from the rep index (not the order of creation), adding
replicates later never changes existing seeds.

## See also

[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)

Other LANDIS-II execution helpers:
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
