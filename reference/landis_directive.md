# Read a directive's value from a LANDIS-II configuration file

LANDIS-II config files are `Directive value` lines with `>>` starting a
comment that runs to end of line, and values quoted only when they
contain whitespace. This returns the first value found for `directive`,
unquoted.

## Usage

``` r
landis_directive(file, directive, default = NA_character_)
```

## Arguments

- file:

  Character path to the configuration file.

- directive:

  Character. The directive name to look up.

- default:

  Value to return when the file is missing or the directive is absent.
  Defaults to `NA_character_`.

## Value

A length-1 character value, or `default`.
