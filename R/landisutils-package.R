utils::globalVariables(c(
  ".", ".data", "active", "age", "age_bin", "age_max", "age_min", "all_files", "anpp_max",
  "anpp_prop", "Area", "as.data.table", "B", "BaseVar", "batch", "batch_map_code", "BatchID",
  "biomass_max", "BUI", "CellID", "cellSize", "cohort_age", "CohortAge", "CohortBiomass",
  "community", "DATE", "DAY", "Day", "DC", "DMC", "EcoID", "ecoregion", "ecoregion_label",
  "ecoregionGroup", "elev", "emfs_ha", "empiricalBurnRate", "establishprob", "everything",
  "FFMC", "FireRegionName", "FireSeverity", "FireTolerance", "firetolerance", "FWI",
  "growth_shp", "GrowthCurve", "growthcurve", "growthCurveSource", "hardsoft", "id",
  "IgnitionProb", "inflationFactor", "inflection", "is_swept", "ISI", "JULIAN_DAY", "k", "KeyID",
  "landis_species", "lat", "LeafLignin", "leafLignin", "LeafLongevity", "leaflongevity",
  "level_source", "level_source_requested", "level_source_used", "lon", "Longevity", "longevity",
  "mANPPproportion", "map_code", "MapCode", "mature_from", "mature_to", "maxANPP", "maxB",
  "MaxSize", "MeanSize", "MinSize", "MONTH", "Month", "mort_shp", "MortalityCurve",
  "mortalityshape", "n_bins", "n_communities", "newAge", "newB", "newPixelGroup", "note",
  "pIgnition", "pixelGroup", "plot_quantile", "plots_sparse", "plots_warn_below", "PolyID",
  "PostFireRegen", "postfireregen", "Prcp", "ProbMortality", "rank", "RelH", "resproutage_max",
  "resproutage_min", "resproutprob", "RH", "SeedDispDistEff", "SeedDispDistMax",
  "seeddistance_eff", "seeddistance_max", "sexualmature", "SexualMaturity", "ShadeTolerance",
  "shadetolerance", "species", "SpeciesCode", "speciesCode", "SpeciesName", "SproutAgeMax",
  "SproutAgeMin", "SRad", "starts_with", "structure_id", "swept_species", "Tmax", "Tmin",
  "total_biomass", "Value", "value", "Variable", "VegReprodProb", "WD", "weight_plots",
  "weight_sortie", "weight_tipsy", "weight_vdyp", "window_source", "WndD", "WndS", "WoodDecayRate",
  "wooddecayrate", "WS", "xBar", "YEAR", "Year", "year"
))

#' @keywords internal
"_PACKAGE"

#' @import methods
NULL

## usethis namespace: start
#' @importFrom callr r_bg
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table as.data.table
#' @importFrom data.table between
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table fwrite
#' @importFrom data.table rbindlist
#' @importFrom data.table rowid
#' @importFrom data.table set
#' @importFrom data.table setcolorder
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom fs dir_create
#' @importFrom fs path_abs
#' @importFrom fs path_norm
#' @importFrom fs path_rel
#' @importFrom fs path_tidy
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom lifecycle deprecated
#' @importFrom R6 R6Class
#' @importFrom stats na.omit
#' @importFrom stringr fixed
#' @importFrom stringr str_c
#' @importFrom stringr str_ends
#' @importFrom terra names
#' @importFrom terra nlyr
#' @importFrom terra rast
#' @importFrom terra res
#' @importFrom terra terrain
#' @importFrom terra values
#' @importFrom terra values<-
#' @importFrom terra writeRaster
#' @importFrom tibble tribble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom utils head
#' @importFrom utils packageVersion
#' @importFrom utils tail
#' @importFrom utils write.csv
## usethis namespace: end
NULL
