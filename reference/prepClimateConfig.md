# Create `ClimateConfigFile`

Create `ClimateConfigFile`

## Usage

``` r
prepClimateConfig(path, ...)
```

## Arguments

- path:

  Character. Path specifying a directory to use for the scenario runs.

- ...:

  additional climate config arguments:

  - `ClimateTimeSeries`;

  - `ClimateFile`;

  - `SpinUpClimateTimeSeries`;

  - `SpinUpClimateFile`;

  - `GenerateClimateOutputFiles`;

  - `UsingFireClimate`;

  - `FineFuelMoistureCode`;

  - `DuffMoistureCode`;

  - `DroughtCode`;

  - `FirstDayFire`;

  - `LastDayFire`;

## Value

`LandisClimateConfig` object

## See also

Used by succession extensions that share a top-level climate library:
[BiomassSuccession](https://for-cast.github.io/landisutils/reference/BiomassSuccession.md),
[DGSSuccession](https://for-cast.github.io/landisutils/reference/DGSSuccession.md),
[NECNSuccession](https://for-cast.github.io/landisutils/reference/NECNSuccession.md),
and (optionally)
[PnETSuccession](https://for-cast.github.io/landisutils/reference/PnETSuccession.md).
