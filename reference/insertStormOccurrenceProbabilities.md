# Specify the `StormOccurrenceProbabilities` block for the Hurricane extension

Specify the `StormOccurrenceProbabilities` block for the Hurricane
extension

## Usage

``` r
insertStormOccurrenceProbabilities(df)
```

## Arguments

- df:

  `data.frame` with columns `Storms`, `Probability`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Hurricane helpers:
[`Hurricane`](https://for-cast.github.io/landisutils/reference/Hurricane.md),
[`defaultHurricaneMortalityCurve()`](https://for-cast.github.io/landisutils/reference/defaultHurricaneMortalityCurve.md),
[`insertExposureMaps()`](https://for-cast.github.io/landisutils/reference/insertExposureMaps.md),
[`insertWindSpeedVulnerabilities()`](https://for-cast.github.io/landisutils/reference/insertWindSpeedVulnerabilities.md),
[`windSpeedVulnerability()`](https://for-cast.github.io/landisutils/reference/windSpeedVulnerability.md)
