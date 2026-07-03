# Write one replicate's biomass_snapshots to a partitioned parquet

Reads a single replicate's `log_BiomassC.csv` via
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)
(Arrow-filtered to `times`, masked to `cell_mask`, cohort ages summed)
and writes it to
`<scenario_dir>/<subdir>/replicate=<rep>/part-0.parquet`, where
`scenario_dir` is inferred as `dirname(dirname(src_path))` and `<rep>`
is the replicate directory. This is the writer counterpart to
[`read_biomass_c_snapshots_for_scenario()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots_for_scenario.md);
colocating the parquet inside the scenario directory means it is
archived alongside the raw replicate outputs and adding a replicate is a
single write with no downstream ripple.

## Usage

``` r
write_biomass_c_snapshots_parquet(
  src_path,
  times,
  cell_mask = NULL,
  subdir = "_aggregates/biomass_snapshots",
  staging_dir = NULL
)
```

## Arguments

- src_path:

  Path to one replicate's `log_BiomassC.csv`.

- times:

  Integer vector of snapshot years (see
  [`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)).

- cell_mask:

  Optional `data.frame` with `row`/`column` columns identifying the
  core-area cells to retain (see
  [`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)).

- subdir:

  Path within the scenario directory for the dataset root (default
  `"_aggregates/biomass_snapshots"`, matching
  [`read_biomass_c_snapshots_for_scenario()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots_for_scenario.md)).

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

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_c_snapshots_for_scenario()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots_for_scenario.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
