# Create a `targets` target that runs one LANDIS-II replicate

A `{targets}` factory that creates **one** `format = "file"` target.
Each branch runs exactly one LANDIS-II simulation and returns only that
replicate's output files.

## Usage

``` r
tar_landis(
  name,
  scenario_dir,
  rep_index,
  deps = NULL,
  scenario_file = "scenario.txt",
  output_dir = "output",
  method = NULL,
  image = NULL,
  console = NULL,
  base_seed = NULL,
  pull = FALSE,
  force = FALSE,
  cpu_limit = 4,
  mem_limit = "8g",
  mem_margin = 1.5,
  post_completion_timeout_sec = 300,
  work_root = NULL,
  pattern = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
)
```

## Arguments

- name:

  Symbol (unquoted). Target name.

- scenario_dir:

  Symbol or expression (unquoted). Upstream target that provides the
  scenario directory path(s) at run time.

- rep_index:

  Symbol (unquoted). Upstream target that provides the 1-based integer
  index for the current branch. Must be defined with
  `iteration = "vector"` so that `cross()` iterates over individual
  elements. Typically created as
  `tar_target(name = ..._rep_index, command = seq_len(n_reps), iteration = "vector")`.

- deps:

  List (unquoted, optional). A
  [`list()`](https://rdrr.io/r/base/list.html) of upstream target
  symbols that must complete before the simulation runs, e.g.
  `list(landis_scenario_file, landis_ext_forcs_file)`. Values are not
  used directly – they are embedded in the command so `{targets}`
  detects them as upstream dependencies.

- scenario_file:

  Character. Scenario filename inside `scenario_dir`.

- output_dir:

  Character vector. Output subdirectory (or subdirectories) inside
  `scenario_dir`; all files found there (recursively) are returned as
  tracked outputs. Defaults to `"output"`. Pass `c("output", "fire")`
  when using the Dynamic Fire extension, which writes its maps and logs
  to a `fire/` subdirectory.

- method:

  Character or `NULL`. `"docker"` to run in Docker, `"local"` to run via
  a local `dotnet` installation. `NULL` (default) reads
  `getOption("landisutils.run.method")`, which itself defaults to
  `"local"` on Windows and `"docker"` on Linux/macOS.

- image:

  Character or `NULL`. Docker image to use (`method = "docker"` only).
  Resolved from `getOption("landisutils.docker.image")` when `NULL`.

- console:

  Character or `NULL`. Path to `Landis.Console.dll`. For
  `method = "docker"` this is the path *inside* the container; for
  `method = "local"` it is the local filesystem path (defaults to
  [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)).

- base_seed:

  Integer or `NULL`. Passed to
  [`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md):
  when non-`NULL`, the `RandomNumberSeed` in each replicate's
  `scenario.txt` is set to `base_seed + (rep_index - 1)`, giving each
  run a distinct but deterministic seed. Adding more replicates later
  never changes existing seeds because seeds are derived from the rep
  index, not the order of creation.

- pull:

  Logical. Passed to
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  when `method = "docker"`: when `TRUE`, the image is `docker pull`ed
  before running so the digest captured in `log/docker_image.log`
  reflects the current registry. Defaults to `FALSE`. No effect for
  `method = "local"`.

- force:

  Logical (default `FALSE`). When `FALSE`, `tar_landis()` skips the
  actual `landis_run_*()` call if the rep dir already contains a
  completed `Landis-log.txt` *and* a `log/input_hash.json` sidecar whose
  recorded hash matches the current inputs (per-input-file MD5 +
  `base_seed`

  - `rep_index` + `scenario_file`). When `TRUE`, the skip check is
    bypassed and LANDIS-II is invoked unconditionally.

- cpu_limit, mem_limit, mem_margin:

  Passed to
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  when `method = "docker"`. See that function's documentation for
  semantics; defaults are `4`, `"8g"`, and `1.5` respectively. No effect
  for `method = "local"`.

- post_completion_timeout_sec:

  Numeric. Passed to
  [`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
  when `method = "docker"`: grace period (seconds) after the LANDIS-II
  console logs `"Model run is complete."` before the container is
  SIGTERMed if `dotnet` has not exited on its own. Guards against the
  known v8 post-completion hang (long ForCS + Dynamic Fire sims spin at
  100% CPU in the .NET shutdown path after outputs are already on disk).
  Default `300` (5 min); pass `Inf` to disable the watchdog. No effect
  for `method = "local"`.

- work_root:

  Character or `NULL`. Optional fast, local, Docker-bind- mountable
  scratch root used to RUN each replicate, separate from the final
  (tracked) `scenario_dir`. Needed when `scenario_dir` lives on storage
  the Docker daemon cannot bind-mount (e.g. a root-squashed NFS share):
  the rep is staged + run under
  `work_root/<scenario>/<studyArea>/repNN`, then its completed outputs
  are moved to `scenario_dir/repNN` via
  [`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md)
  (fault-tolerant `rsync` + retry) so the value the target returns – and
  everything `{targets}` tracks – is the FINAL location, with scratch
  holding only transient run files. The skip-check and output collection
  both read the final `scenario_dir/repNN`. When `NULL` (default), the
  per-run env var `LANDIS_SCRATCH` is consulted at run time (so `{crew}`
  workers can set it via `.Rprofile`); when that is also empty the rep
  runs in place under `scenario_dir` (original behaviour). No effect for
  `method = "local"`.

- pattern:

  Expression (unquoted). Dynamic-branching pattern covering both the
  scenario and replicate dimensions, e.g.
  `cross(landis_run_name, landis_run_output_rep_index)`. Passed directly
  to
  [`targets::tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html).

- packages, library, error, memory, resources, storage, retrieval, cue,
  description:

  Standard `{targets}` options; all default to
  [`targets::tar_option_get()`](https://docs.ropensci.org/targets/reference/tar_option_get.html).

## Value

A single `tar_target` object (from
[`targets::tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html)).

## Details

**Per-replicate parallel branching:** the caller creates a rep-index
target with `iteration = "vector"` and combines it with the scenario
dimension via `cross()`. Keeping the rep-index target explicit in the
project's [`list()`](https://rdrr.io/r/base/list.html) makes it visible
to static analysis tools (tarborist) and makes the
`iteration = "vector"` annotation clear in the project code:

    list(
      ## iteration = "vector" is critical: cross() iterates over each ELEMENT,
      ## giving n_scenarios x n_reps independent branches dispatched in parallel.
      tar_target(
        name      = landis_run_output_rep_index,
        command   = seq_len(5L),
        iteration = "vector"
      ),
      landisutils::tar_landis(
        name      = landis_run_output,
        rep_index = landis_run_output_rep_index,
        ...
        pattern   = cross(landis_run_name, landis_run_output_rep_index)
      )
    )

**Caching:** each (scenario, replicate) branch is cached independently.
Adding replicates (increasing the
[`seq_len()`](https://rdrr.io/r/base/seq.html) value) only computes new
branches; existing ones remain untouched.

The `method` is resolved at *factory-call time* (i.e., when `_targets.R`
is evaluated) and baked into the target command. This ensures that
`{crew}` worker processes, which do not inherit R session options,
receive the correct values. Set `options(landisutils.run.method = ...)`
in `_local.R` before the pipeline list is constructed to control the
method per machine.

## See also

[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
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
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
