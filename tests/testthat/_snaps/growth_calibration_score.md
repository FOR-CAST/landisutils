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

