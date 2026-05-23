# Create a `targets` target that runs LANDIS-II

A `{targets}` factory that creates one `format = "file"` target per
scenario (via dynamic branching). The target runs LANDIS-II — locally or
inside a Docker container depending on `method` — and returns a
character vector of tracked output and log files.

## Usage

``` r
tar_landis(
  name,
  scenario_dir,
  deps = NULL,
  scenario_file = "scenario.txt",
  output_dir = "output",
  method = NULL,
  image = NULL,
  console = NULL,
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

- deps:

  List (unquoted, optional). A
  [`list()`](https://rdrr.io/r/base/list.html) of upstream target
  symbols that must complete before the simulation runs, e.g.
  `list(landis_scenario_file, landis_ext_forcs_file)`. Values are not
  used directly — they are embedded in the command so `{targets}`
  detects them as upstream dependencies.

- scenario_file:

  Character. Scenario filename inside `scenario_dir`.

- output_dir:

  Character. Output subdirectory inside `scenario_dir`; all files found
  there (recursively) are returned as tracked outputs.

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

- pattern:

  Expression (unquoted, optional). Dynamic-branching pattern, e.g.
  `map(landis_run_name)`. Passed to
  [`targets::tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html).

- packages, library, error, memory, resources, storage, retrieval, cue,
  description:

  Standard `{targets}` options; all default to
  [`targets::tar_option_get()`](https://docs.ropensci.org/targets/reference/tar_option_get.html).

## Value

A `tar_target` object (from
[`targets::tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html)).

## Details

The `method` is resolved at *factory-call time* (i.e., when `_targets.R`
is evaluated) and baked into the target command. This ensures that
`{crew}` worker processes, which do not inherit R session options,
receive the correct values. Set `options(landisutils.run.method = ...)`
in `_local.R` before the pipeline list is constructed to control the
method per machine.

## See also

[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)

Other LANDIS-II execution helpers:
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
