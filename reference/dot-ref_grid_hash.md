# Stable id for a fixed climate reference grid (namespaces the global BioSIM cache).

Hashes the grid's geometry (extent, resolution, CRS code, cell count) so
the same reference grid always maps to the same cache namespace – the
basis for cross-study-area reuse + accumulation.

## Usage

``` r
.ref_grid_hash(ref_grid)
```
