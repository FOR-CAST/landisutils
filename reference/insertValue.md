# Specify an input parameter value for an extension

Specify an input parameter value for an extension

## Usage

``` r
insertValue(type, value, blank_line = TRUE)
```

## Arguments

- type:

  Character, specifying the parameter type (i.e., the LANDIS-II name for
  the input).

- value:

  A single parameter value of integer, numeric, or character type.

- blank_line:

  Logical. Should a blank line be added after the line?

## Value

Character string(s) to write to the suitable LANDIS-II input file.
