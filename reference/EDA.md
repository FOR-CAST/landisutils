# Epidemiological Disturbance Agents (EDA) Extension

Epidemiological Disturbance Agents (EDA) Extension

## References

LANDIS-II Base EDA v3 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Base-EDA/blob/master/docs/LANDIS-II%20Epidemiological%20Disturbance%20Agent%20v3%20User%20Guide.pdf>

## See also

Other EDA helpers:
[`edaAgent()`](https://for-cast.github.io/landisutils/reference/edaAgent.md),
[`insertEDADisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertEDADisturbanceModifiers.md),
[`insertEDAIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertEDAIgnoredSpecies.md),
[`insertEDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertEDAInputFiles.md),
[`insertEDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertEDASpeciesParameters.md),
[`writeEDAAgentFile()`](https://for-cast.github.io/landisutils/reference/writeEDAAgentFile.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `EDA`

## Active bindings

- `Agents`:

  List of `EDAAgent` objects.

- `MapNames`:

  Character. Output map filename pattern; must contain the literal
  `{agentName}` and `{timestep}` placeholders – LANDIS-II replaces them
  with the agent name and simulation year, e.g.
  `"eda/{agentName}-{timestep}.tif"`.

- `MORTMapNames`:

  (Optional) Character. Mortality output map filename pattern; must
  contain the literal `{agentName}` and `{timestep}` placeholders, e.g.
  `"eda/{agentName}-MORT-{timestep}.tif"`.

- `LogFile`:

  Character. Relative file path.

- `output_files`:

  Character vector of output files (relative paths from the scenario
  directory) that this extension is expected to produce at run time.
  Subclasses override this to return their extension-specific log files,
  event CSVs, etc. These paths are collected by
  [`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
  and written to `output_manifest.txt` so
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
  can track them explicitly without relying on
  [`list.files()`](https://rdrr.io/r/base/list.files.html) discovery.

  Map files whose names depend on the timestep (e.g. `BiomassC-10.tif`)
  are NOT included here; they are discovered by
  [`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)'s
  `output_dir` scan instead.

## Methods

### Public methods

- [`EDA$new()`](#method-EDA-initialize)

- [`EDA$add_agent()`](#method-EDA-add_agent)

- [`EDA$write()`](#method-EDA-write)

- [`EDA$clone()`](#method-EDA-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `EDA$new()`

#### Usage

    EDA$new(
      path,
      Timestep = NULL,
      Agents = list(),
      MapNames = NULL,
      MORTMapNames = NULL,
      LogFile = "eda/eda-log.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between updates.

- `Agents`:

  List of `EDAAgent` objects (see
  [`edaAgent()`](https://for-cast.github.io/landisutils/reference/edaAgent.md)).

- `MapNames`:

  Character. Output map filename pattern; must contain the literal
  `{agentName}` and `{timestep}` placeholders.

- `MORTMapNames`:

  (Optional) Character. Mortality output map filename pattern; must
  contain the literal `{agentName}` and `{timestep}` placeholders.

- `LogFile`:

  Character. Relative file path for the EDA CSV log.

------------------------------------------------------------------------

### `EDA$add_agent()`

#### Usage

    EDA$add_agent(value)

#### Arguments

- `value`:

  `EDAAgent` object to append to `$Agents`.

------------------------------------------------------------------------

### `EDA$write()`

Write extension inputs to disk

#### Usage

    EDA$write()

------------------------------------------------------------------------

### `EDA$clone()`

The objects of this class are cloneable with this method.

#### Usage

    EDA$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
