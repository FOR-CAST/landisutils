# Repair and re-derive a table of BioSIM `FWI_Daily` records

The single entry point for making BioSIM `FWI_Daily` output trustworthy.
Used by
[`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md),
and exported so that projects with their own BioSIM fetch apply exactly
the same correction rather than reimplementing it.

## Usage

``` r
repair_fwi_daily(fwi_daily, validate = TRUE)
```

## Arguments

- fwi_daily:

  data frame of `FWI_Daily` records with at least `FFMC`, `DMC`, `DC`
  and `WS`.

- validate:

  logical. Check the repaired values against physical bounds and error
  on a violation. Default `TRUE`; a violation means a failure mode the
  repair does not characterise, which must not reach a weather database
  silently.

## Value

`fwi_daily` with `FFMC`, `DMC` and `DC` repaired and `BUI`, `ISI` and
`FWI` re-derived.

## Details

Two steps:

1.  **Repair the three moisture codes** `FFMC`, `DMC` and `DC` in place,
    via
    [`repair_fwi_exponent()`](https://for-cast.github.io/landisutils/reference/repair_fwi_exponent.md).
    These are recursive, precipitation-driven daily codes, and
    `FWI_Daily` does not return precipitation in every configuration, so
    they cannot in general be recomputed from the other columns and must
    be recovered as they stand. Repairing `DMC` and `DC` matters as much
    as `FFMC`: a corrupt `DMC` otherwise flows into the buildup index
    and from there into `FWI`.

2.  **Discard and re-derive the three behaviour indices** `BUI`, `ISI`
    and `FWI` from the repaired codes using `cffdrs`. These carry the
    same artifact as the codes – `BUI` drops below `1e-4` whenever `DMC`
    does – but they are pure functions of the codes, so recomputing is
    strictly safer than repairing: it needs no threshold and cannot
    mistake a genuine extreme for corruption. Nothing is lost by doing
    so. On uncorrupted records the recomputed values reproduce BioSIM's
    own to within `5e-6` relative for `BUI` and 0.8% for `ISI` and
    `FWI`.

Records BioSIM did not model (`-9999` sentinels, which callers map to
`NA` before calling) pass through as `NA`.

## See also

[`repair_fwi_exponent()`](https://for-cast.github.io/landisutils/reference/repair_fwi_exponent.md),
[`get_fwi_daily()`](https://for-cast.github.io/landisutils/reference/get_fwi_daily.md)
