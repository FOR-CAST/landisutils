# Repair BioSIM's sign-dropped scientific-notation exponents

`BioSIM` returns the Fire Weather Index System codes and indices as
text, and drops the minus sign from the exponent whenever a value is
small enough to be formatted in scientific notation (below `1e-4`). A
true `6.49936E-05` comes back as `6.49936E+05`: the mantissa is intact
and only the exponent's sign is lost, so `v * 10^(-2 * floor(log10(v)))`
recovers the original value exactly. Verified against the FWI System
equations – `FFMC = 17.1624, WS = 8.96132` gives `ISI = 6.49934e-05`,
and `BioSIM` returned `649936`.

## Usage

``` r
repair_fwi_exponent(v)
```

## Arguments

- v:

  numeric vector of a single FWI System code.

## Value

`v` with artifact values restored; all other values untouched.

## Details

Only saturated fuels drive the codes below `1e-4`, so the artifact is
confined to wet, low-hazard records, but it inflates them by up to
eighteen orders of magnitude. Any downstream mean over cells or days is
destroyed by a handful of contaminated records, so this must be applied
before the values are aggregated or used to derive indices.

Applied to the three moisture codes (`FFMC`, `DMC`, `DC`), which
`BioSIM` reports directly and which cannot be recomputed from the other
returned columns without a precipitation series. The behaviour indices
(`ISI`, `BUI`, `FWI`) are pure functions of those codes and are
re-derived from the repaired values by
[`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
instead.

## See also

[`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
