# Robust `BioSIM::generateWeather()` wrapper: request staggering + exponential-backoff retry

The BioSIM web API can be slow or transiently unavailable when many
requests arrive at once, and the `BioSIM` R client exposes no
timeout/retry/delay knobs (`biosimclient.config()` only tunes
`nbNearestNeighbours` and climate-generation/test flags). A bare
`generateWeather()` call therefore has no protection: a transient
failure aborts the whole fetch, and a stalled call hangs indefinitely.
This wrapper adds (all tunable via
[`options()`](https://rdrr.io/r/base/options.html)):

- a random pre-request **stagger delay** so concurrent `furrr` workers
  don't hit the server in lockstep – `landisutils.biosim.request_delay`
  = `c(min, max)` seconds (default `c(1, 5)`);

- **exponential backoff with jitter** on failure –
  `landisutils.biosim.max_attempts` (default `5L`) total tries, waiting
  `landisutils.biosim.backoff_base * 2^(attempt - 1)` seconds (base
  default `15`) between tries, so repeated failures back off
  progressively (gentler on an overwhelmed server);

- an optional **per-attempt timeout** – `landisutils.biosim.timeout`
  seconds (default `Inf` = off) via
  [`base::setTimeLimit()`](https://rdrr.io/r/base/setTimeLimit.html); on
  timeout the J4R client is reset
  ([`BioSIM::shutdownClient()`](https://rdrr.io/pkg/BioSIM/man/shutdownClient.html))
  before the next try so a stalled socket can't poison it.

## Usage

``` r
.biosim_generate_weather(...)
```

## Arguments

- ...:

  passed verbatim to
  [`BioSIM::generateWeather()`](https://rdrr.io/pkg/BioSIM/man/generateWeather.html).

## Value

the
[`BioSIM::generateWeather()`](https://rdrr.io/pkg/BioSIM/man/generateWeather.html)
result; errors after exhausting `max_attempts`.

## Details

NOTE: these options are read at call time INSIDE the fetch (which often
runs in a `furrr`/`crew` worker), so to tune them set the
[`options()`](https://rdrr.io/r/base/options.html) somewhere the worker
inherits – e.g. the project `.Rprofile` – NOT a file only the main
process sources (a `targets` `_local.R`).
