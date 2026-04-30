# Convert logical to a LANDIS-II yes/no or true/false token

LANDIS-II input files mostly use the strings `"yes"` and `"no"` for
boolean parameters, but a few extensions (e.g. Magic Harvest's
`NoHarvestReInitialization`) require `"true"` / `"false"` instead.

## Usage

``` r
yesno(x)

truefalse(x)
```

## Arguments

- x:

  one of `TRUE`, `FALSE`, `"Y"`, `"N"`, `"yes"`, `"no"`, `"T"`, `"F"`,
  `"true"`, `"false"` (case-insensitive).

## Value

- `yesno()`: character `"yes"` or `"no"`.

- `truefalse()`: character `"true"` or `"false"`.
