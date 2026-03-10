# Create LANDIS-II scenario files

Create LANDIS-II scenario files

## Usage

``` r
scenario(
  name = NULL,
  extensions = NULL,
  climate_config = NULL,
  path = NULL,
  ...
)
```

## Arguments

- name:

  Character. Label to use as a filename and label for this scenario.

- extensions:

  List of
  [LandisExtension](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
  objects.

- climate_config:

  [LandisClimateConfig](https://for-cast.github.io/landisutils/reference/LandisClimateConfig.md)
  object.

- path:

  Character. Path specifying a directory to use for the scenario runs.

- ...:

  Arguments passed to other functions:

  - `CellLength` Numeric. Size of ecoregion raster cells (in \$m\$);

  - `DisturbancesRandomOrder` Logical. Should disturbances be applied in
    a random order?

  - `Duration` Numeric. Number of years to run the simulation;

  - `EcoregionsFiles` List of length 2 containing character file paths;

  - `RandomNumberSeed` Integer. Seed used to initialize the LANDIS-II
    random number generator;

  - `SpeciesInputFile` Character. Path to input file;

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.
