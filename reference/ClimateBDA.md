# Climate BDA (Biological Disturbance Agent) Extension

Climate BDA (Biological Disturbance Agent) Extension

## References

LANDIS-II Biological Disturbance Agent v5 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Base-BDA/blob/master/docs/LANDIS-II%20Biological%20Disturbance%20Agent%20v5%20User%20Guide.pdf>

## See also

Other Climate BDA helpers:
[`bdaAgent()`](https://for-cast.github.io/landisutils/reference/bdaAgent.md),
[`insertAgentHeader()`](https://for-cast.github.io/landisutils/reference/insertAgentHeader.md),
[`insertBDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertBDAInputFiles.md),
[`insertBDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertBDASpeciesParameters.md),
[`insertDispersal()`](https://for-cast.github.io/landisutils/reference/insertDispersal.md),
[`insertDisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceModifiers.md),
[`insertEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertEcoregionModifiers.md),
[`insertIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertIgnoredSpecies.md),
[`insertIntensityClasses()`](https://for-cast.github.io/landisutils/reference/insertIntensityClasses.md),
[`insertNeighborhood()`](https://for-cast.github.io/landisutils/reference/insertNeighborhood.md),
[`insertOutbreakPattern()`](https://for-cast.github.io/landisutils/reference/insertOutbreakPattern.md),
[`writeAgentFile()`](https://for-cast.github.io/landisutils/reference/writeAgentFile.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `ClimateBDA`

## Active bindings

- `Agents`:

  List of `BDAAgent` objects.

- `MapNames`:

  Character. Output map filename pattern; must contain the literal
  `{agentName}` and `{timestep}` placeholders.

- `SRDMapNames`:

  (Optional) Character. SRD output map filename pattern; must contain
  `{agentName}` and `{timestep}`.

- `NRDMapNames`:

  (Optional) Character. NRD output map filename pattern; must contain
  `{agentName}` and `{timestep}`.

- `BDPMapNames`:

  (Optional) Character. BDP output map filename pattern; must contain
  `{agentName}` and `{timestep}`.

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

- [`ClimateBDA$new()`](#method-ClimateBDA-initialize)

- [`ClimateBDA$add_agent()`](#method-ClimateBDA-add_agent)

- [`ClimateBDA$write()`](#method-ClimateBDA-write)

- [`ClimateBDA$clone()`](#method-ClimateBDA-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `ClimateBDA$new()`

#### Usage

    ClimateBDA$new(
      path,
      Timestep = NULL,
      Agents = list(),
      MapNames = NULL,
      SRDMapNames = NULL,
      NRDMapNames = NULL,
      BDPMapNames = NULL,
      LogFile = "bda/bda-log.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `Agents`:

  List of `BDAAgent` objects (see
  [`bdaAgent()`](https://for-cast.github.io/landisutils/reference/bdaAgent.md)).

- `MapNames`:

  Character. Output map filename pattern; must contain the literal
  `{agentName}` and `{timestep}` placeholders.

- `SRDMapNames`:

  (Optional) Character. SRD output map filename pattern; must contain
  the literal `{agentName}` and `{timestep}` placeholders.

- `NRDMapNames`:

  (Optional) Character. NRD output map filename pattern; must contain
  the literal `{agentName}` and `{timestep}` placeholders.

- `BDPMapNames`:

  (Optional) Character. BDP output map filename pattern; must contain
  the literal `{agentName}` and `{timestep}` placeholders.

- `LogFile`:

  Character. Relative file path for the BDA CSV log.

------------------------------------------------------------------------

### `ClimateBDA$add_agent()`

#### Usage

    ClimateBDA$add_agent(value)

#### Arguments

- `value`:

  `BDAAgent` object to append to `$Agents`.

------------------------------------------------------------------------

### `ClimateBDA$write()`

Write extension inputs to disk

#### Usage

    ClimateBDA$write()

------------------------------------------------------------------------

### `ClimateBDA$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ClimateBDA$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
