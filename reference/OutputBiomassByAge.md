# Biomass Output by Age Extension

Biomass Output by Age Extension

Biomass Output by Age Extension

## References

LANDIS-II Biomass-by-Age Output v4.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-By-Age/blob/master/docs/LANDIS-II%20Biomass-by-Age%20Output%20v4.0%20User%20Guide.pdf>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputBiomassByAge`

## Active bindings

- `MapNames`:

  Character. File pattern for writing outputs to disk.

- `Species`:

  Character vector of species names with age classes.

## Methods

### Public methods

- [`OutputBiomassByAge$new()`](#method-OutputBiomassByAge-new)

- [`OutputBiomassByAge$write()`](#method-OutputBiomassByAge-write)

- [`OutputBiomassByAge$clone()`](#method-OutputBiomassByAge-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OutputBiomassByAge$new(
      path = NULL,
      Timestep = 10L,
      MapNames = NULL,
      Species = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `MapNames`:

  Character. File pattern for writing outputs to disk.

- `Species`:

  Character vector of species names with age classes.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    OutputBiomassByAge$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputBiomassByAge$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
biomass_by_age <- OutputBiomassByAge$new(
  path = tempdir(),
  Timestep = 10,
  MapNames = NULL, # use default
  Species = c(
    "pinubank ageclass1(10-40) ageclass2(15-100)",
    "pinuresi ageclass(>200)",
    "pinustro ageclass(>250)",
    "poputrem ageclass1(<50)"
  )
)

biomass_by_age$write()
```
