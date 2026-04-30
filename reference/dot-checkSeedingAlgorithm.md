# Validate a `SeedingAlgorithm` value

Internal helper used by the active bindings of every succession
extension that exposes a `SeedingAlgorithm` field. Raises an informative
error if `value` is not one of `.seedingAlgorithms`.

## Usage

``` r
.checkSeedingAlgorithm(value)
```

## Arguments

- value:

  Character. Candidate value.

## Value

`value`, invisibly, when valid.
