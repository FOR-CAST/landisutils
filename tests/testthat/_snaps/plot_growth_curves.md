# plot_species_growth_curves errors on missing required columns

    Code
      plot_species_growth_curves(data.frame(species = 1), data.frame(speciesTemp = 1))
    Condition
      Error in `plot_species_growth_curves()`:
      ! all(c("species", "standAge", "BscaledNonLinear") %in% names(landis_curves)) is not TRUE

