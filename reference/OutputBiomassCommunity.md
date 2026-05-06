# Biomass Community Output Extension

Biomass Community Output Extension

## References

LANDIS-II Output Biomass Community v3 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Community/blob/master/docs/LANDIS-II%20Output%20Biomass%20Community%20v3%20User%20Guide.pdf>

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OutputBiomassCommunity`

## Methods

### Public methods

- [`OutputBiomassCommunity$new()`](#method-OutputBiomassCommunity-initialize)

- [`OutputBiomassCommunity$write()`](#method-OutputBiomassCommunity-write)

- [`OutputBiomassCommunity$clone()`](#method-OutputBiomassCommunity-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `OutputBiomassCommunity$new()`

#### Usage

    OutputBiomassCommunity$new(path, Timestep = 10L)

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between community snapshots.

------------------------------------------------------------------------

### `OutputBiomassCommunity$write()`

Write extension inputs to disk

#### Usage

    OutputBiomassCommunity$write()

------------------------------------------------------------------------

### `OutputBiomassCommunity$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OutputBiomassCommunity$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
bio_community <- OutputBiomassCommunity$new(path = tempdir(), Timestep = 10)
bio_community$write()
```
