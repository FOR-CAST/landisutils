# Move a completed LANDIS-II replicate from scratch to its final location

Moves a finished replicate directory `run_dir` (typically on fast,
local, Docker-bind-mountable scratch) to `final_dir` (typically a
slower, networked final/archive location such as an NFS project share)
in a way that is fault-tolerant *and* all-or-nothing at the destination:

## Usage

``` r
landis_archive_rep(run_dir, final_dir, max_tries = 5L, backoff_sec = 5)
```

## Arguments

- run_dir:

  Character. The completed replicate directory to move (source).

- final_dir:

  Character. The destination directory (created if needed).

- max_tries:

  Integer. Maximum `rsync` attempts before giving up (default `5`).

- backoff_sec:

  Numeric. Base seconds for linear backoff between attempts (attempt `i`
  waits `backoff_sec * i`; default `5`).

## Value

Character. `final_dir`, on success.

## Details

1.  `rsync -a --partial` copies `run_dir` into a sibling staging
    directory `paste0(final_dir, ".partial")` on the *destination*
    filesystem, with retry + linear backoff so a transient network blip
    does not abort the run (`--partial` keeps partially transferred
    files so a retry resumes rather than restarts). Crucially this is a
    COPY – the scratch source stays the complete, authoritative
    replicate until the destination is verified, so a total transfer
    failure loses nothing.

2.  After a verified `rsync` exit status of `0`, the staging directory
    is published with an atomic `rename` into `final_dir`. Because the
    rename is atomic, `final_dir` only ever appears *complete* – a
    partial transfer is never visible to a downstream skip-check that
    reads `final_dir`. (This is why `--remove-source-files` is
    deliberately NOT used: it would delete scratch incrementally and
    could leave a half-emptied source on total failure, and a partial
    destination could be mistaken for a complete run.)

3.  Only once `final_dir` is in place is the scratch source deleted.

If `run_dir` and `final_dir` already resolve to the same path (no
scratch in use), this is a no-op that returns `final_dir` without
copying or deleting.

Requires the `rsync` executable on `PATH` (standard on Linux/macOS). A
missing `rsync`, or repeated failures, raises an error after `max_tries`
attempts so the run target fails loudly and the scratch copy is retained
for inspection rather than silently lost.

## See also

[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
