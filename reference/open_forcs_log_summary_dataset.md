# Open the ForCS `log_Summary` Arrow dataset for one or more scenarios

Opens each existing `<scenario>/_aggregates/forcs_log_summary` root as
an Arrow dataset partitioned by `replicate`. A single root opens
directly; multiple roots under different parents are opened individually
and combined into a `UnionDataset` (Arrow cannot treat Hive trees under
different parents as one dataset). Missing roots (e.g. scenarios whose
runs errored) are dropped.

## Usage

``` r
open_forcs_log_summary_dataset(dataset_roots)
```

## Arguments

- dataset_roots:

  Character vector of `<scenario>/_aggregates/forcs_log_summary` paths.

## Value

An Arrow `Dataset` (lazy), or `NULL` if no roots exist.

## See also

Other ForCS output helpers:
[`read_forcs_log_summary()`](https://for-cast.github.io/landisutils/reference/read_forcs_log_summary.md),
[`write_forcs_log_summary_parquet()`](https://for-cast.github.io/landisutils/reference/write_forcs_log_summary_parquet.md)
