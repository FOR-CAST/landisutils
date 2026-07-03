# Write one replicate's ForCS `log_Summary` to a partitioned parquet

Reads one replicate's `log_Summary.csv` via
[`read_forcs_log_summary()`](https://for-cast.github.io/landisutils/reference/read_forcs_log_summary.md),
masks to core cells, and writes it to
`<scenario_dir>/<subdir>/replicate=<rep>/part-0.parquet`, where
`scenario_dir` is inferred as `dirname(dirname(src_path))`. This is the
writer counterpart to
[`open_forcs_log_summary_dataset()`](https://for-cast.github.io/landisutils/reference/open_forcs_log_summary_dataset.md);
`scenario` is embedded as a data column so several per-scenario roots
can be unioned and filtered without touching the directory layout.

## Usage

``` r
write_forcs_log_summary_parquet(
  src_path,
  cell_mask = NULL,
  subdir = "_aggregates/forcs_log_summary",
  staging_dir = NULL
)
```

## Arguments

- src_path:

  Path to one replicate's `log_Summary.csv`.

- cell_mask:

  Optional `data.frame` with `row`/`column` columns identifying the
  core-area cells to retain (see
  [`read_forcs_log_summary()`](https://for-cast.github.io/landisutils/reference/read_forcs_log_summary.md)).

- subdir:

  Path within the scenario directory for the dataset root (default
  `"_aggregates/forcs_log_summary"`, matching
  [`open_forcs_log_summary_dataset()`](https://for-cast.github.io/landisutils/reference/open_forcs_log_summary_dataset.md)).

- staging_dir:

  Optional directory for the temporary parquet before it is moved into
  place; `NULL` (default) stages in the destination directory
  (same-filesystem atomic rename).

## Value

The written parquet path.

## Details

The publish is atomic: the parquet is written to a temporary file and
then [`fs::file_move()`](https://fs.r-lib.org/reference/file_move.html)d
into place, so a concurrent reader or a retried write never observes a
partial file – safe for many replicate writers running at once against
an NFS output directory. When `staging_dir` is supplied the temporary is
written there (e.g. per-host scratch, keeping the interim bytes off NFS)
and moved cross-filesystem; the default stages in the destination
directory so the move is a same-filesystem atomic rename.

## See also

Other ForCS output helpers:
[`open_forcs_log_summary_dataset()`](https://for-cast.github.io/landisutils/reference/open_forcs_log_summary_dataset.md),
[`read_forcs_log_summary()`](https://for-cast.github.io/landisutils/reference/read_forcs_log_summary.md)
