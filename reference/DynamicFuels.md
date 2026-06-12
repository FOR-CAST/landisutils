# Dynamic Fuel System Extension

Dynamic Fuel System Extension

## References

LANDIS-II Dynamic Fuel System Extension v4.0 User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Biomass-Fuels/blob/master/docs/LANDIS-II%20Dynamic%20Fuel%20System%20v4%20User%20Guide.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/prepFuelTypesTable.md),
[`prepDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceConversionTable.md).

Other Dynamic Fuels helpers:
[`insertDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceConversionTable.md),
[`insertEcoregionTable()`](https://for-cast.github.io/landisutils/reference/insertEcoregionTable.md),
[`insertFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/insertFuelTypesTable.md),
[`insertSpeciesFuelCoefficients()`](https://for-cast.github.io/landisutils/reference/insertSpeciesFuelCoefficients.md),
[`prepDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceConversionTable.md),
[`prepFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/prepFuelTypesTable.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `DynamicFuels`

## Active bindings

- `SpeciesFuelCoefficients`:

  `data.frame`.

- `HardwoodMaximum`:

  Integer.

- `DeadFirMaxAge`:

  Integer. The duration of influence for the BDA extension's dead
  conifer index.

- `FuelTypes`:

  `data.frame`. See
  [`prepFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/prepFuelTypesTable.md).

- `EcoregionTable`:

  `data.frame`.

- `DisturbanceConversionTable`:

  `data.frame`.

- `MapFileNames`:

  Character. File pattern for writing outputs to disk; must contain the
  literal `{timestep}` placeholder (the only variable the upstream
  Dynamic Fuels `MapNames` parser knows – see `MapFileNames.cs`).

- `PctConiferFileName`:

  Character. File pattern for writing outputs to disk; must contain the
  literal `{timestep}` placeholder.

- `PctDeadFirFileName`:

  Character. File pattern for writing outputs to disk; must contain the
  literal `{timestep}` placeholder.

## Methods

### Public methods

- [`DynamicFuels$new()`](#method-DynamicFuels-initialize)

- [`DynamicFuels$write()`](#method-DynamicFuels-write)

- [`DynamicFuels$clone()`](#method-DynamicFuels-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `DynamicFuels$new()`

#### Usage

    DynamicFuels$new(
      path,
      Timestep = 10,
      SpeciesFuelCoefficients = NULL,
      HardwoodMaximum = 0L,
      DeadFirMaxAge = 15L,
      FuelTypes = NULL,
      EcoregionTable = data.frame(FuelType = integer(0), Ecoregion = character(0)),
      DisturbanceConversionTable = NULL,
      MapFileNames = NULL,
      PctConiferFileName = NULL,
      PctDeadFirFileName = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `SpeciesFuelCoefficients`:

  `data.frame`.

- `HardwoodMaximum`:

  Integer.

- `DeadFirMaxAge`:

  Integer. The duration of influence for the BDA extension's dead
  conifer index.

- `FuelTypes`:

  `data.frame`. See
  [`prepFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/prepFuelTypesTable.md).

- `EcoregionTable`:

  `data.frame`.

- `DisturbanceConversionTable`:

  `data.frame`.

- `MapFileNames`:

  Character. File pattern for writing outputs to disk.

- `PctConiferFileName`:

  Character. File pattern for writing outputs to disk.

- `PctDeadFirFileName`:

  Character. File pattern for writing outputs to disk.

------------------------------------------------------------------------

### `DynamicFuels$write()`

Write extension inputs to disk

#### Usage

    DynamicFuels$write()

------------------------------------------------------------------------

### `DynamicFuels$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DynamicFuels$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
