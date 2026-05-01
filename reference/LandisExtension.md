# `LandisExtension` class

Keeps track of the input files created for an extension

## Public fields

- `path`:

  Character specifying the (directory) path to the extension input
  files.

- `files`:

  Character specifying the filenames of the extension input files
  (relative to `path`); the principle extension input file should be
  listed first.

## Active bindings

- `LandisData`:

  Character. The extension's `LandisData` entry (e.g.
  `"Biomass Succession"`). Read-only; set by each subclass's
  `initialize()` via `private$.LandisData`.

- `scenarioName`:

  Character. The name to use when this extension is referenced in the
  scenario `.txt` file's extension list. Defaults to `LandisData`;
  subclasses set `private$.scenarioName` only when the parser-registered
  name differs from the file-header name (e.g. Land Use Plus).
  Read-only.

- `Timestep`:

  Integer.

- `type`:

  character specifying the extension type (must be one of: succession,
  disturbance, other)

## Methods

### Public methods

- [`LandisExtension$new()`](#method-LandisExtension-new)

- [`LandisExtension$add_file()`](#method-LandisExtension-add_file)

- [`LandisExtension$clone()`](#method-LandisExtension-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LandisExtension$new(
      LandisData = NULL,
      type = NA_character_,
      path = NA_character_,
      Timestep = 1L
    )

#### Arguments

- `LandisData`:

  Character. The extension's `LandisData` entry (e.g.
  `"Biomass Succession"`).

- `type`:

  Character specifying the extension type (must be one of: succession,
  disturbance, other)

- `path`:

  Character specifying the (directory) path to the extension input
  files.

- `Timestep`:

  Integer.

------------------------------------------------------------------------

### Method `add_file()`

#### Usage

    LandisExtension$add_file(value)

#### Arguments

- `value`:

  if specified, the new value to append to `files`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LandisExtension$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
