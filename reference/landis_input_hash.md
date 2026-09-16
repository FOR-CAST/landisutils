# Input hash of one LANDIS-II replicate

A digest of what a replicate was run from: the MD5 of every dependency
file (keyed by path), the base seed, the replicate index and the
scenario file name.
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
writes it to `<rep>/log/input_hash.json` after a run and compares it
before the next, via
[`landis_rep_is_current()`](https://for-cast.github.io/landisutils/reference/landis_rep_is_current.md).

## Usage

``` r
landis_input_hash(
  dep_files,
  base_seed,
  rep_index,
  scenario_file,
  collation = NULL
)
```

## Arguments

- dep_files:

  Character. The dependency files staged into the replicate.

- base_seed:

  Integer. The base random seed.

- rep_index:

  Integer. The replicate index.

- scenario_file:

  Character(1). The scenario file name.

- collation:

  `NULL` (the default) for byte order. Otherwise an `LC_COLLATE` locale
  to sort in, which reproduces the hash landisutils 0.0.149 and earlier
  wrote in that locale; `""` means the locale the process's environment
  selects, as when R started.

## Value

Character(1), a SHA-1 digest; `NA_character_` if `collation` cannot be
set.

## Details

Paths are ordered by bytes (`sort(method = "radix")`), so the hash does
not depend on the collation locale of the R process computing it. R
collates with ICU in UTF-8 locales and by bytes under C/POSIX; a
locale-sorted hash differs between a worker started with and without
`LANG`.

R also collates by bytes, whatever
[`Sys.setlocale()`](https://rdrr.io/r/base/locales.html) says, while the
environment variable `LC_ALL` (or, when that is unset, `LC_COLLATE`) is
`C`. For a named `collation` the sort therefore runs with `LC_ALL` unset
and `LC_COLLATE` set to `collation`, as in a process started in that
locale, and both are restored afterwards.
