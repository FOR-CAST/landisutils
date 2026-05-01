# Specify an input file for an extension

Emits a `<type> <file>` line. `NULL` (and length-zero) input is treated
as "parameter omitted" and produces no output, matching how
[`insertValue()`](https://for-cast.github.io/landisutils/reference/insertValue.md)
handles `NA`.

## Usage

``` r
insertFile(type, file)
```

## Arguments

- type:

  Character, specifying the parameter type (i.e., the LANDIS-II name for
  the input).

- file:

  Character, specifying the path to the file.

## Value

Character string(s) to write to the suitable LANDIS-II input file.
