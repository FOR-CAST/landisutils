# Magic Harvest Extension

Magic Harvest Extension

Magic Harvest Extension

## Details

Wraps an external script invocation that mutates a Biomass Harvest
parameter file at each timestep.

## References

LANDIS-II Magic Harvest v2 Extension User Guide
<https://github.com/Klemet/LANDIS-II-Magic-Harvest>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `MagicHarvest`

## Active bindings

- `HarvestExtensionParameterFile`:

  Character. Relative path to the wrapped harvest extension's parameter
  file.

- `ProcessToLaunch`:

  Character. Program name to execute.

- `ProcessArguments`:

  Character. Arguments passed to the process.

- `NoHarvestReInitialization`:

  Character `"true"` or `"false"` (set via logical or
  `"true"`/`"false"`). The Magic Harvest extension parser only accepts
  boolean tokens here, not `yes`/`no`.

## Methods

### Public methods

- [`MagicHarvest$new()`](#method-MagicHarvest-new)

- [`MagicHarvest$write()`](#method-MagicHarvest-write)

- [`MagicHarvest$clone()`](#method-MagicHarvest-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    MagicHarvest$new(
      path,
      Timestep = NULL,
      HarvestExtensionParameterFile = NULL,
      ProcessToLaunch = NULL,
      ProcessArguments = NULL,
      NoHarvestReInitialization = FALSE
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between Magic Harvest invocations.

- `HarvestExtensionParameterFile`:

  Character. Relative path to the wrapped harvest extension's parameter
  file (e.g. a Biomass Harvest `.txt`).

- `ProcessToLaunch`:

  Character. Program name to execute (e.g. `"python"`, `"Rscript"`);
  must be callable from the shell.

- `ProcessArguments`:

  Character. Arguments passed to the process; may contain a literal
  `{timestep}` placeholder. Use `""` or `"{none}"` for no arguments.

- `NoHarvestReInitialization`:

  Logical (or `"true"`/`"false"`). When `TRUE`, skips re-initialization
  of the wrapped harvest extension between timesteps. Optional; defaults
  to `FALSE`. LANDIS-II's Magic Harvest parser requires the literal
  token `true` or `false` here (not `yes`/`no`); see
  [`truefalse()`](https://for-cast.github.io/landisutils/reference/yesno.md).

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    MagicHarvest$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MagicHarvest$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
