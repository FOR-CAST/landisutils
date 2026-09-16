# Is a LANDIS-II replicate already complete for these inputs?

`TRUE` when `Landis-log.txt` reports a completed run and
`log/input_hash.json` records the input hash of `dep_files` et al.
Besides the current
[`landis_input_hash()`](https://for-cast.github.io/landisutils/reference/landis_input_hash.md),
the hash landisutils 0.0.149 and earlier would have written is accepted,
sorted in this process's locale or under `C.UTF-8` (ICU). A replicate
finished by an earlier version is therefore not re-simulated after an
upgrade, whichever of those locales wrote it.

## Usage

``` r
landis_rep_is_current(
  rep_dir,
  dep_files,
  base_seed,
  rep_index,
  scenario_file,
  force = FALSE
)
```

## Arguments

- rep_dir:

  Character(1). The replicate directory.

- dep_files:

  Character. The dependency files staged into the replicate.

- base_seed:

  Integer. The base random seed.

- rep_index:

  Integer. The replicate index.

- scenario_file:

  Character(1). The scenario file name.

- force:

  Logical(1). `TRUE` never counts a replicate as current.

## Value

Logical(1).
