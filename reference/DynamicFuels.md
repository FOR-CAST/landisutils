# Dynamic Fuel System Extension

Dynamic Fuel System Extension

Dynamic Fuel System Extension

## References

LANDIS-II Dynamic Fuel System Extension v4.0 User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Biomass-Fuels/blob/master/docs/LANDIS-II%20Dynamic%20Fuel%20System%20v4.0%20User%20Guide.pdf>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
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

  Character. File pattern for writing outputs to disk.

- `PctConiferMapName`:

  Character. File pattern for writing outputs to disk.

- `PctDeadFirMapName`:

  Character. File pattern for writing outputs to disk.

## Methods

### Public methods

- [`DynamicFuels$new()`](#method-DynamicFuels-new)

- [`DynamicFuels$write()`](#method-DynamicFuels-write)

- [`DynamicFuels$clone()`](#method-DynamicFuels-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

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
      PctConiferMapName = NULL,
      PctDeadFirMapName = NULL
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

- `PctConiferMapName`:

  Character. File pattern for writing outputs to disk.

- `PctDeadFirMapName`:

  Character. File pattern for writing outputs to disk.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    DynamicFuels$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DynamicFuels$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
