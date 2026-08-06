# Require an optional (`Suggests`) package, with an actionable error

Several capabilities here rest on packages that most users of this one
never touch: reading Arrow datasets, drawing plots, recomputing fire
weather. Those live in `Suggests` rather than `Imports` so installing
this package does not drag them in – `arrow` alone is 99 MB, and
`cffdrs` pulls 16 packages nothing else needs. Call this at the top of
any exported function that reaches one, so a missing package produces a
message naming what to install rather than a bare "there is no package
called ..." from somewhere deep in the call stack.

## Usage

``` r
.need(pkg, what)
```

## Arguments

- pkg:

  Character. The package needed.

- what:

  Character. What the caller was trying to do, phrased to read after a
  subject: `"Reading a parquet dataset"`.

## Value

Invisibly `TRUE`; called for its side effect of stopping.
