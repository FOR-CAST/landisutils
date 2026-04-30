# Format a vector of keyword names for inline `roxygen2` use

Wraps each element in backticks (so `roxygen2` markdown renders each key
as `\code{}` in the generated `.Rd`, which also keeps these identifiers
out of
[`spelling::spell_check_package()`](https://docs.ropensci.org/spelling//reference/spell_check_package.html))
and joins with `, `. Intended for inline expansion via
`` `r .fmtKeys(.someKeys)` `` so users see the literal allowed values in
the rendered manual rather than a reference to a non-exported internal
vector.

## Usage

``` r
.fmtKeys(x)
```

## Arguments

- x:

  Character vector of keyword names.

## Value

A single character string.
