# Specify a Linear Wind intensity table (`TornadoIntensityTable` or `DerechoIntensityTable`)

Emits 5 rows with intensity class labels `0.2, 0.4, 0.6, 0.8, 1.0` and
the supplied percentages (which must sum to 100).

## Usage

``` r
insertLinearWindIntensityTable(name, pct)
```

## Arguments

- name:

  Character. Either `"TornadoIntensityTable"` or
  `"DerechoIntensityTable"`.

- pct:

  Numeric vector of length 5.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Linear Wind helpers:
[`LinearWind`](https://for-cast.github.io/landisutils/reference/LinearWind.md),
[`defaultLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultLinearWindSeverities.md),
[`insertLinearWindEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertLinearWindEcoregionModifiers.md),
[`insertLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/insertLinearWindSeverities.md),
[`insertWindDirectionTable()`](https://for-cast.github.io/landisutils/reference/insertWindDirectionTable.md)
