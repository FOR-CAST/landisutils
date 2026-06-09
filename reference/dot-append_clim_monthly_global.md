# Append newly-fetched climate-only rows to the global monthly store (partitioned by YEAR).

Uses a content-derived basename so concurrent/serial appends never
clobber existing partition files; callers only ever fetch MISSING cells,
so no de-duplication is required on read.

## Usage

``` r
.append_clim_monthly_global(df, dataset_dir, year)
```
