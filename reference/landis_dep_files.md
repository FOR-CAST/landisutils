# Resolve a replicate's dependency files against its own scenario directory

Exported because
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
emits a target command that calls it, and generated code cannot reach an
unexported name without `:::`. Not part of the user-facing API.

## Usage

``` r
landis_dep_files(deps, scenario_dir)
```

## Arguments

- deps:

  List of upstream target values; character elements are treated as file
  paths.

- scenario_dir:

  Character. The replicate's scenario directory.

## Value

Character vector of files to stage, one per basename.
