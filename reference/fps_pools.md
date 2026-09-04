# Summarise FPSM output into annual carbon stocks and atmospheric emissions

Reduces the per-transfer raw output to one row per
`(scenario, replicate, year)`.

## Usage

``` r
fps_pools(raw, drop_terminal_year = TRUE)
```

## Arguments

- raw:

  Tibble from
  [`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md).

- drop_terminal_year:

  Logical. Drop years after the last annual stock report. `FALSE` keeps
  them; read the note above before doing so.

## Value

A tibble with `scenario`, `replicate`, `year`, and the tonnes of carbon
in products (`products_tC`) and in special pools such as landfills and
dumps (`special_pools_tC`), plus atmospheric emissions in that year as
carbon (`emitted_co2_tC`, `emitted_ch4_tC`). Emissions are carbon, not
CO2e and not CH4 mass; convert downstream.

## Details

**The terminal year is not comparable to the others and is dropped by
default.** FPSM writes its annual end-of-year stock reports (types 4 and
5) up to the second-to-last simulated year, then emits a different,
partial set of residual rows for the final year: types 1 and 2 only, and
only for the pools that decay. In the shipped complex example the
special-pool stock is 225 t C in year 19, and year 20 carries no type 4
or 5 row at all: this summary would report 0 there, while a naive sum
over every retained row would report 76 t C. Both are artefacts of that
reporting change rather than a real collapse, so carrying the final year
into a stock time series draws a cliff that did not happen. The cut is
derived from the data – the last year with a type 4 or 5 report – not
from a hard-coded year.

## See also

Other FPSM helpers:
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
[`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md),
[`fps_stocks_by_pool()`](https://for-cast.github.io/landisutils/reference/fps_stocks_by_pool.md),
[`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
[`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md),
[`write_fps_raw_out_parquet()`](https://for-cast.github.io/landisutils/reference/write_fps_raw_out_parquet.md)
