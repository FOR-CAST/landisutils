# Biomass Output Extension

Biomass Output Extension

Biomass Output Extension

## References

LANDIS-II Output Biomass v4.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass/blob/master/docs/LANDIS-II%20Output%20Biomass%20v4.0%20User%20Guide.pdf>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputBiomass`

## Active bindings

- `MakeTable`:

  Logical, or character indicating "yes" or "no".

- `Species`:

  Character vector of species names, or "all".

- `LiveMapNames`:

  Character. File pattern for writing outputs to disk.

- `DeadPools`:

  Character. One of "woody", "non-woody", or "both".

- `DeadMapNames`:

  Character. File pattern for writing outputs to disk.

## Methods

### Public methods

- [`OutputBiomass$new()`](#method-OutputBiomass-new)

- [`OutputBiomass$write()`](#method-OutputBiomass-write)

- [`OutputBiomass$clone()`](#method-OutputBiomass-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OutputBiomass$new(
      path = NULL,
      Timestep = 10L,
      MakeTable = NULL,
      Species = "all",
      LiveMapNames = NULL,
      DeadPools = "both",
      DeadMapNames = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `MakeTable`:

  Logical, or character indicating "yes" or "no".

- `Species`:

  Character vector of species names, or "all".

- `LiveMapNames`:

  Character. File pattern for writing outputs to disk.

- `DeadPools`:

  Character. One of "woody", "non-woody", or "both".

- `DeadMapNames`:

  Character. File pattern for writing outputs to disk.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    OutputBiomass$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputBiomass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
output_biomass <- OutputBiomass$new(
  path = tempdir(),
  Timestep = 10,
  MakeTable = "yes",
  Species = "all",
  LiveMapNames = NULL, # use default
  DeadPools = "both",
  DeadMapNames = NULL # use default
)

output_biomass$write()
```
