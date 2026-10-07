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

Character vector of files to stage, one per basename. When `deps` holds
files of other scenarios, they are attached, in `deps` order, as the
attribute `"foreign"`, which
[`landis_rep_is_current()`](https://for-cast.github.io/landisutils/reference/landis_rep_is_current.md)
uses to recognise a replicate staged by landisutils 0.0.168 or earlier.

## Details

A file inside another directory beside `scenario_dir` (a sibling under
the same parent) belongs to another scenario and is not staged. Every
other file – under `scenario_dir`, directly in its parent, or outside
the parent – is staged, one per basename, with files under
`scenario_dir` first. Inputs that several scenarios share must therefore
not sit in a sibling directory.
