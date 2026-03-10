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
