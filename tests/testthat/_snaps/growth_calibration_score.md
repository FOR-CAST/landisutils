# smoothing errors on a missing site column rather than silently skipping

    Code
      growth_smooth_observations(obs, site = "nope")
    Condition
      Error:
      ! `site` column 'nope' not found in `obs`.

# a missing site column is an error, not a silent skip

    Code
      growth_bin_observations(obs, site = "site")
    Condition
      Error:
      ! `site` column 'site' not found in `obs`.

# a level recovered from a curve that never plateaued warns

    Code
      best <- growth_best_candidates(scores, current, window_aa)
    Condition
      Warning:
      level recovered by extrapolation for Aa: the best candidate reached under 90% of its own asymptote, so `biomass_max_est` is scaled up by 2x. Check `biomass_max_lo`/`biomass_max_hi` before promoting it.

# palette overrides apply, and a typo is an error rather than a no-op

    Code
      growth_plot_palette(candiate = "darkorange")
    Condition
      Error:
      ! unknown palette role(s): candiate. Known roles: current, candidate, plots, faint, summary, reference, window, key_outline.

---

    Code
      growth_plot_palette("darkorange")
    Condition
      Error:
      ! overrides must be named, e.g. growth_plot_palette(candidate = 'red').

