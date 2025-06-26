utils::globalVariables(c(
  ".", "active", "age", "all_files", "Area", "as.data.table", "B", "cellSize", "CohortAge",
  "CohortBiomass", "community", "Day", "ecoregionGroup", "emfs_ha", "empiricalBurnRate",
  "establishprob", "everything", "FireRegionName", "FireSeverity", "firetolerance",
  "FireTolerance", "growthcurve", "GrowthCurve", "growthCurveSource", "hardsoft",
  "IgnitionProb", "inflationFactor", "k", "leafLignin", "LeafLignin", "leaflongevity",
  "LeafLongevity", "longevity", "Longevity", "mANPPproportion", "MapCode", "maxANPP",
  "maxB", "MaxSize", "MeanSize", "MinSize", "Month", "MortalityCurve", "mortalityshape",
  "newAge", "newB", "newPixelGroup", "pIgnition", "pixelGroup",
  "PolyID", "postfireregen", "PostFireRegen", "ProbMortality", "resproutage_max",
  "resproutage_min", "resproutprob", "SeedDispDistEff", "SeedDispDistMax",
  "seeddistance_eff", "seeddistance_max", "sexualmature", "SexualMaturity",
  "shadetolerance", "ShadeTolerance", "species", "speciesCode", "SpeciesCode",
  "SpeciesName", "SproutAgeMax", "SproutAgeMin", "starts_with", "Value", "VegReprodProb",
  "wooddecayrate", "WoodDecayRate", "xBar", "year", "Year"
))

#' @keywords internal
"_PACKAGE"

#' @import methods
NULL

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table between
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom data.table fifelse
#' @importFrom data.table fwrite
#' @importFrom data.table rbindlist
#' @importFrom data.table set
#' @importFrom data.table setcolorder
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#' @importFrom dplyr all_of
#' @importFrom dplyr mutate
#' @importFrom fs dir_create
#' @importFrom fs dir_exists
#' @importFrom fs dir_ls
#' @importFrom fs file_exists
#' @importFrom fs path_norm
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom stats na.omit
#' @importFrom terra names
#' @importFrom terra nlyr
#' @importFrom terra rast
#' @importFrom terra res
#' @importFrom terra values
#' @importFrom terra values<-
#' @importFrom terra writeRaster
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom utils head
#' @importFrom utils packageVersion
#' @importFrom utils tail
#' @importFrom utils write.csv
## usethis namespace: end
NULL
