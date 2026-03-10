# `LandisScenario` class

Keeps track of a scenario's files and all its extensions' input files.

## Public fields

- `path`:

  character specifying the (directory) path to the scenario file

- `files`:

  character specifying the filenames of the scenario files (relative to
  `path`)

- `extensions`:

  list of `LandisExtension` objects

## Active bindings

- `reps`:

  integer number of replicates created for this scenario

## Methods

### Public methods

- [`LandisScenario$new()`](#method-LandisScenario-new)

- [`LandisScenario$add_file()`](#method-LandisScenario-add_file)

- [`LandisScenario$list_extensions()`](#method-LandisScenario-list_extensions)

- [`LandisScenario$list_files()`](#method-LandisScenario-list_files)

- [`LandisScenario$replicate()`](#method-LandisScenario-replicate)

- [`LandisScenario$clone()`](#method-LandisScenario-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LandisScenario$new(path = NA_character_, extensions = list())

#### Arguments

- `path`:

  character specifying the (directory) path to the scenario file

- `extensions`:

  list of `LandisExtension` objects

- `file`:

  character specifying the filename of the scenario file (relative to
  `path`)

------------------------------------------------------------------------

### Method `add_file()`

#### Usage

    LandisScenario$add_file(value)

#### Arguments

- `value`:

  if specified, the new value to append to `files`

------------------------------------------------------------------------

### Method `list_extensions()`

#### Usage

    LandisScenario$list_extensions(type)

#### Arguments

- `type`:

  character specifying the extension type (must be one of: succession,
  disturbance, other)

------------------------------------------------------------------------

### Method `list_files()`

#### Usage

    LandisScenario$list_files(full.names = TRUE)

#### Arguments

- `full.names`:

  logical indicating whether full absolute file paths should be returned

------------------------------------------------------------------------

### Method [`replicate()`](https://rdrr.io/r/base/lapply.html)

#### Usage

    LandisScenario$replicate(n)

#### Arguments

- `n`:

  integer specifying the number of replicates to generate

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LandisScenario$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
