# Epidemiological Disturbance Agents (EDA) Extension

Epidemiological Disturbance Agents (EDA) Extension

## References

LANDIS-II Base EDA v3 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Base-EDA/blob/master/docs/LANDIS-II%20Base%20EDA%20v3%20User%20Guide.pdf>

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
