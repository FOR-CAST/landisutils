# Create replicate directories for a LANDIS-II scenario

Creates numbered replicate sub-directories inside `scenario_dir`
(`rep01/`, `rep02/`, ...) and copies input files into each.

## Usage

``` r
landis_replicate(
  scenario_dir,
  n_reps = NULL,
  rep_index = NULL,
  files = NULL,
  base_seed = NULL
)
```

## Arguments

- scenario_dir:

  Character. Path to the base scenario directory.

- n_reps:

  Integer or `NULL`. Number of replicates to create (all-at-once mode).
  Exactly one of `n_reps` and `rep_index` must be provided.

- rep_index:

  Integer or `NULL`. 1-based index of the single replicate to create
  (single-rep mode). Exactly one of `n_reps` and `rep_index` must be
  provided.

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
  indices are 1-based (`rep01` -\> `base_seed`, `rep02` -\>
  `base_seed + 1`, ...).

## Value

Character vector of absolute paths to the created replicate directories
(length 1 in single-rep mode, length `n_reps` in all-at-once mode), in
replicate order.

## Details

Two modes:

- **All-at-once** (`n_reps`): creates `rep01` through `repNN`. Used by
  legacy code and interactive workflows.

- **Single-rep** (`rep_index`): creates exactly one directory for the
  given 1-based index. Used by
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  when each replicate is a separate targets branch; branches run
  concurrently and each sets up only its own directory, avoiding the
  O(N_REPS) re-setup cost of all-at-once.

If `files` is supplied it is used as the explicit copy list; GDAL
sidecar files (`.tfw`, `.aux.xml`) that accompany any `.tif` in the list
are included automatically. When `files` is `NULL` the fallback is every
top-level file in `scenario_dir` (sub-directories are never copied). The
function is idempotent: existing replicate directories are left
untouched, so adding more replicates later never alters
previously-created ones.

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
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
