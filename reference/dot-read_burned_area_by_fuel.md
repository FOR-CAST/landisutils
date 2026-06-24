# Per-rep cell-based burn area by fuel code (internal)

Walks `<rep_dir>/fire/severity-{t}.tif` and the matching
`<rep_dir>/fire/FuelType-{t}.tif` files, masks the fuel raster to cells
with severity \> 1 (the Dynamic Fire encoding is 0 = inactive, 1 =
active-but-unburned, \>= 2 = burned with the value as severity class),
and accumulates cell counts per fuel code across timesteps. Each
cell-timestep is counted once, so a cell that burns in two distinct
timesteps contributes twice – matching NBAC's per-fire-perimeter
accounting on the observed side.

## Usage

``` r
.read_burned_area_by_fuel(rep_dir, pixel_area_ha)
```

## Details

Returns NULL when:

- `<rep_dir>/fire/` does not exist (caller didn't run LANDIS),

- no `severity-*.tif` files were emitted (no fires in this rep), or

- no matching `FuelType-*.tif` companion is on disk (Dynamic Fuels
  either not enabled or wrote to a different output dir than expected).

Downstream
[`loss_from_stats()`](https://for-cast.github.io/landisutils/reference/loss_from_stats.md)
-\>`.chi_sq_area_by_fuel()` prefers this tibble when present and falls
back to the legacy `events$init_fuel` attribution when it's NULL (e.g.
from mock-simulator trials or payloads written by older landisutils
versions).
