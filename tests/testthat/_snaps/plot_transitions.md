# write_biomass_c_snapshots_parquet errors on empty input

    Code
      write_biomass_c_snapshots_parquet("rep01/log_BiomassC.csv", times = 9999L)
    Condition
      Error:
      ! write_biomass_c_snapshots_parquet(): empty input for rep01/log_BiomassC.csv

