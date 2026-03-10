# `LandisClimateConfig` class

Keeps track of the input files created for the climate library

## Public fields

- `path`:

  character specifying the (directory) path to the extension input files

- `files`:

  character specifying the filenames of the extension input files
  (relative to `path`); the principle extension input file should be
  listed first.

## Methods

### Public methods

- [`LandisClimateConfig$new()`](#method-LandisClimateConfig-new)

- [`LandisClimateConfig$add_file()`](#method-LandisClimateConfig-add_file)

- [`LandisClimateConfig$clone()`](#method-LandisClimateConfig-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LandisClimateConfig$new(path = NA_character_)

#### Arguments

- `path`:

  character specifying the (directory) path to the extension input files

------------------------------------------------------------------------

### Method `add_file()`

#### Usage

    LandisClimateConfig$add_file(value)

#### Arguments

- `value`:

  if specified, the new value to append to `files`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LandisClimateConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
