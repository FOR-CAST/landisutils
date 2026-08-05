# a missing site column is an error, not a silent skip

    Code
      growth_bin_observations(obs, site = "site")
    Condition
      Error:
      ! `site` column 'site' not found in `obs`.

