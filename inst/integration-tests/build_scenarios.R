#!/usr/bin/env Rscript

## Build runnable LANDIS-II scenarios for integration testing.
##
## Each scenario uses upstream LANDIS-II Foundation reference inputs (downloaded
## at run time from the relevant extension repository's `testings/` directory)
## as the data files (initial communities, ecoregions, species, climate, ...),
## and uses the {landisutils} package API to (re)generate the LANDIS-II
## scenario file and per-extension config files on top of that data.
##
## A LANDIS-II run on each resulting directory therefore validates the syntax
## of the package-generated config files against the LANDIS-II console parser.
##
## Usage: Rscript inst/integration-tests/build_scenarios.R <output-dir>
##   On exit, prints the absolute path of each scenario subdirectory to stdout
##   (one per line), so the caller can iterate over them.

suppressPackageStartupMessages({
  library(landisutils)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("usage: build_scenarios.R <output-dir>", call. = FALSE)
}
out_dir <- args[1]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir, mustWork = TRUE)

message(sprintf("Building scenarios in: %s", out_dir))

download_refs <- function(base_url, dest_dir, filenames) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  for (f in filenames) {
    src <- paste0(base_url, "/", f)
    dst <- file.path(dest_dir, f)
    message(sprintf("  fetching: %s", f))
    rc <- utils::download.file(src, dst, mode = "wb", quiet = TRUE)
    if (rc != 0L || !file.exists(dst)) {
      stop(sprintf("failed to download %s", src), call. = FALSE)
    }
  }
}

scenarios <- character(0)

## ---------------------------------------------------------------------------
## Scenario: Biomass Succession (no disturbances, no outputs)
##
## Reference data: LANDIS-II-Foundation/Extension-Biomass-Succession,
##   testings/CoreV8.0-BiomassSuccession7.0
## ---------------------------------------------------------------------------
{
  scen_name <- "biomass_succession"
  scen_path <- file.path(out_dir, scen_name)

  ref_base <- paste0(
    "https://raw.githubusercontent.com/",
    "LANDIS-II-Foundation/Extension-Biomass-Succession/master/",
    "testings/CoreV8.0-BiomassSuccession7.0"
  )
  ref_files <- c(
    "Core_species_data.txt",
    "SpeciesData.csv",
    "SppEcoregionData.csv",
    "biomass-succession_ClimateGenerator.txt",
    "biomass-succession_InitialCommunities.csv",
    "ecoregions.tif",
    "ecoregions.txt",
    "initial-communities.tif",
    ## referenced by biomass-succession_ClimateGenerator.txt (ClimateFile +
    ## SpinUpClimateFile); LANDIS-II resolves this relative to the scenario dir.
    "PRISM_data_AFRI_4.18.13_v2.csv"
  )
  download_refs(ref_base, scen_path, ref_files)

  ## Values mirror the upstream reference biomass-succession.txt so that they
  ## are compatible with the reference SpeciesData.csv / SppEcoregionData.csv /
  ## ecoregions.txt the file points at.
  min_rel_b <- tibble::tribble(
    ~ShadeClass , ~Eco1 , ~Eco2 ,
    NA_integer_ , "101" , "102" ,
    1L          , "25%" , "25%" ,
    2L          , "45%" , "45%" ,
    3L          , "56%" , "56%" ,
    4L          , "70%" , "70%" ,
    5L          , "90%" , "90%"
  )

  suff_light <- tibble::tribble(
    ~class , ~X0 , ~X1 , ~X2  , ~X3  , ~X4  , ~X5  ,
    1L     , 1.0 , 0.5 , 0.25 , 0.0  , 0.0  , 0.0  ,
    2L     , 1.0 , 1.0 , 0.5  , 0.25 , 0.0  , 0.0  ,
    3L     , 1.0 , 1.0 , 1.0  , 0.5  , 0.25 , 0.0  ,
    4L     , 1.0 , 1.0 , 1.0  , 1.0  , 0.5  , 0.25 ,
    5L     , 0.1 , 0.5 , 1.0  , 1.0  , 1.0  , 1.0
  )

  erp_df <- tibble::tribble(
    ~ecoregion , ~AET ,
    "101"      ,  600 ,
    "102"      ,  600
  )

  frp_df <- tibble::tribble(
    ~Severity , ~WoodLitterReduct , ~LitterReduct ,
    1L        , 0.0               , 0.5           ,
    2L        , 0.0               , 0.75          ,
    3L        , 0.0               , 1.0
  )

  hrp_df <- tibble::tribble(
    ~Name            , ~WoodLitterReduct , ~LitterReduct , ~CohortWoodRemoval , ~CohortLeafRemoval ,
    "MaxAgeClearcut" , 0.5               , 0.15          , 0.8                , 0.0                ,
    "PatchCutting"   , 1.0               , 1.0           , 1.0                , 0.0
  )

  ext_bs <- BiomassSuccession$new(
    path = scen_path,
    Timestep = 10,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesFiles = c(
      file.path(scen_path, "biomass-succession_InitialCommunities.csv"),
      file.path(scen_path, "initial-communities.tif")
    ),
    ClimateConfigFile = file.path(scen_path, "biomass-succession_ClimateGenerator.txt"),
    CalibrateMode = NULL,
    SpinupCohorts = FALSE,
    SpinupMortalityFraction = 0.05,
    MinRelativeBiomass = min_rel_b,
    SufficientLight = suff_light,
    SpeciesDataFile = file.path(scen_path, "SpeciesData.csv"),
    EcoregionParameters = erp_df,
    SpeciesEcoregionDataFile = file.path(scen_path, "SppEcoregionData.csv"),
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df
  )
  ext_bs$write()

  climate_cfg <- LandisClimateConfig$new(path = scen_path)
  climate_cfg$add_file("biomass-succession_ClimateGenerator.txt")

  scen <- scenario(
    name = scen_name,
    extensions = list(ext_bs),
    climate_config = climate_cfg,
    path = scen_path,
    CellLength = 100,
    DisturbancesRandomOrder = FALSE,
    Duration = 10,
    EcoregionsFiles = c(
      file.path(scen_path, "ecoregions.txt"),
      file.path(scen_path, "ecoregions.tif")
    ),
    RandomNumberSeed = 147,
    SpeciesInputFile = file.path(scen_path, "Core_species_data.txt")
  )

  scenarios <- c(scenarios, scen$path)
}

## ---------------------------------------------------------------------------
## Scenario: Biomass Succession + the extensions installed in the
##           `landis-ii-v8-release` docker image.
##
## The image bundles a curated subset of the LANDIS-II ecosystem; extensions
## like Deer Browse, Root Rot, EDA, Hurricane, LandUsePlus, OutputLandscape-
## Habitat, OutputBiomassPnET and PnETSuccession are NOT registered in it, so
## including them here would cause LANDIS-II to abort scenario parsing with
## "No extension with the name ...". This scenario therefore only exercises:
##   - OutputBiomassCommunity                      (other)
##   - OutputLocalHabitat / OutputWildlifeHabitat  (other; suitabilityFile())
##
## Two image-supported extensions parse cleanly but need fuller fixtures
## before they will run end-to-end, so they are intentionally omitted here:
##   - MagicHarvest: parser accepts the package-generated `magic-harvest.txt`
##     (the `NoHarvestReInitialization` field correctly emits the boolean
##     token via `truefalse()`), but the extension's `Run()` needs both an
##     actual wrapped harvest extension (e.g. Biomass Harvest) in the
##     scenario and a real `ProcessToLaunch` / `ProcessArguments` pair
##     pointing at an external script.
##   - ForestRoadsSimulation: parser accepts the package-generated config,
##     but the extension's `Initialize()` needs ecologically meaningful
##     raster inputs (a buildable-zone map and an initial road network
##     with sawmill/main-road cells) plus a harvest extension to drive
##     road construction.
##
## Reuses the same reference inputs as the first scenario (downloaded again
## into its own directory). The intent is to exercise `$write()` on each
## supported class against the LANDIS-II console parser; the ecological
## inputs themselves are skeletal.
## ---------------------------------------------------------------------------
{
  scen_name <- "biomass_succession_plus_extras"
  scen_path <- file.path(out_dir, scen_name)

  download_refs(ref_base, scen_path, ref_files)

  ext_bs2 <- BiomassSuccession$new(
    path = scen_path,
    Timestep = 10,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesFiles = c(
      file.path(scen_path, "biomass-succession_InitialCommunities.csv"),
      file.path(scen_path, "initial-communities.tif")
    ),
    ClimateConfigFile = file.path(scen_path, "biomass-succession_ClimateGenerator.txt"),
    CalibrateMode = NULL,
    SpinupCohorts = FALSE,
    SpinupMortalityFraction = 0.05,
    MinRelativeBiomass = min_rel_b,
    SufficientLight = suff_light,
    SpeciesDataFile = file.path(scen_path, "SpeciesData.csv"),
    EcoregionParameters = erp_df,
    SpeciesEcoregionDataFile = file.path(scen_path, "SppEcoregionData.csv"),
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df
  )
  ext_bs2$write()

  ## Suitability files are loaded by the habitat output extensions; pull a
  ## real example from each upstream extension's test/ directory.
  download_refs(
    paste0(
      "https://raw.githubusercontent.com/",
      "LANDIS-II-Foundation/Extension-Local-Habitat-Suitability-Output/master/",
      "test/Core8-LocalHabitat3.0"
    ),
    scen_path,
    c("AgeClass_ForestType_example.txt")
  )
  download_refs(
    paste0(
      "https://raw.githubusercontent.com/",
      "LANDIS-II-Foundation/Extension-Output-Wildlife-Habitat/master/",
      "test/Core8-Wildlife3.0"
    ),
    scen_path,
    c("BBWOAgeForestType.txt")
  )

  ext_bc <- OutputBiomassCommunity$new(path = scen_path, Timestep = 10L)
  ext_bc$write()

  ext_lh <- OutputLocalHabitat$new(
    path = scen_path,
    Timestep = 10L,
    OutputTimestep = 10L,
    SuitabilityFiles = c("AgeClass_ForestType_example.txt")
  )
  ext_lh$write()

  ext_wh <- OutputWildlifeHabitat$new(
    path = scen_path,
    Timestep = 10L,
    OutputTimestep = 10L,
    SuitabilityFiles = c("BBWOAgeForestType.txt")
  )
  ext_wh$write()

  climate_cfg2 <- LandisClimateConfig$new(path = scen_path)
  climate_cfg2$add_file("biomass-succession_ClimateGenerator.txt")

  scen2 <- scenario(
    name = scen_name,
    extensions = list(ext_bs2, ext_bc, ext_lh, ext_wh),
    climate_config = climate_cfg2,
    path = scen_path,
    CellLength = 100,
    DisturbancesRandomOrder = FALSE,
    Duration = 10,
    EcoregionsFiles = c(
      file.path(scen_path, "ecoregions.txt"),
      file.path(scen_path, "ecoregions.tif")
    ),
    RandomNumberSeed = 147,
    SpeciesInputFile = file.path(scen_path, "Core_species_data.txt")
  )

  scenarios <- c(scenarios, scen2$path)
}

## ===========================================================================
## Per-image extension registry & helpers for AllExtension-style scenarios.
##
## Source of truth for which extensions ship in each Docker image is the pair
## of YAMLs in `LANDIS-II-Foundation/Tool-Docker-Apptainer`.
## Fetched at build time and translated from upstream repo names to this
## package's R6 class names via `EXT_REPO_TO_CLASS` below.
## ===========================================================================

## Pinned to a specific commit of LANDIS-II-Foundation/Tool-Docker-Apptainer
## for reproducibility; bump when upstream test fixtures change. The same SHA
## is used for both the YAMLs and the test-input tarballs so the registry and
## the inputs come from a consistent snapshot of upstream.
TDA_REPO <- "LANDIS-II-Foundation/Tool-Docker-Apptainer"
TDA_REF <- "6a546fbb55f722751f78925089784152378eebb0" ## 2026-04-29

##' Mapping from upstream extension-repo name (the value of `repo:` in the
##' YAMLs) to the package's R6 class name (matching `R6Class()` definitions in
##' `R/ext_*.R`). When upstream adds a new extension, add a row here.
EXT_REPO_TO_CLASS <- c(
  "Extension-Biomass-Browse" = "BiomassBrowse",
  "Extension-Biomass-Harvest" = "BiomassHarvest",
  "Extension-Biomass-Hurricane" = "Hurricane",
  "Extension-Biomass-Succession" = "BiomassSuccession",
  "Extension-Base-BDA" = "ClimateBDA",
  "Extension-Base-Fire" = "OriginalFire",
  "Extension-Base-Wind" = "OriginalWind",
  "Extension-DGS-Succession" = "DGSSuccession",
  "Extension-Dynamic-Biomass-Fuels" = "DynamicFuels",
  "Extension-Dynamic-Fire-System" = "DynamicFire",
  "Extension-ForCS-Succession" = "ForCS",
  "Extension-Land-Use-Plus" = "LandUsePlus",
  "Extension-LinearWind" = "LinearWind",
  "Extension-Local-Habitat-Suitability-Output" = "OutputLocalHabitat",
  "Extension-NECN-Succession" = "NECNSuccession",
  "Extension-Output-Biomass" = "OutputBiomass",
  "Extension-Output-Biomass-By-Age" = "OutputBiomassByAge",
  "Extension-Output-Biomass-Community" = "OutputBiomassCommunity",
  "Extension-Output-Biomass-PnET" = "OutputBiomassPnET",
  "Extension-Output-Biomass-Reclass" = "OutputBiomassReclass",
  "Extension-Output-Cohort-Statistics" = "OutputCohortStats",
  "Extension-Output-Landscape-Habitat" = "OutputLandscapeHabitat",
  "Extension-Output-Max-Species-Age" = "OutputMaxSpeciesAge",
  "Extension-Output-Wildlife-Habitat" = "OutputWildlifeHabitat",
  "Extension-PnET-Succession" = "PnETSuccession",
  "Extension-Root-Rot" = "RootRot",
  "Extension-Social-Climate-Fire" = "SocialClimateFire",
  "LANDIS-II-Forest-Roads-Simulation-extension" = "ForestRoadsSimulation",
  "LANDIS-II-Magic-Harvest" = "MagicHarvest"
)

##' Fetch a Tool-Docker-Apptainer extension YAML at the pinned ref and return
##' the (uncommented) `repo:` values -- i.e. the repos actually built into the
##' image. The YAML format is a flat list of `- repo: <name>` / `org:` /
##' `commit:` triples; commented entries (lines starting with `#`) are
##' implicitly skipped because the regex requires `^-` at column 0.
fetch_image_extensions <- function(yaml_filename) {
  url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s", TDA_REPO, TDA_REF, yaml_filename)
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  message(sprintf("  fetching: %s", url))
  rc <- utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  if (rc != 0L || !file.exists(tmp)) {
    stop(sprintf("failed to download %s", url), call. = FALSE)
  }
  lines <- readLines(tmp, warn = FALSE)
  active <- grep("^-\\s+repo:\\s+", lines, value = TRUE)
  trimws(sub("^-\\s+repo:\\s+", "", active))
}

##' Translate a vector of upstream repo names into R6 class names via
##' EXT_REPO_TO_CLASS. Warns about any repo that lacks a mapping (these are
##' silently dropped from the allowlist; add the row to EXT_REPO_TO_CLASS
##' once a corresponding R6 class lands in `R/`).
repos_to_classes <- function(repos, mapping = EXT_REPO_TO_CLASS) {
  unknown <- setdiff(repos, names(mapping))
  if (length(unknown) > 0L) {
    warning(
      sprintf(
        "%d upstream extension repo(s) have no R6-class mapping (skipped): %s",
        length(unknown),
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unname(mapping[intersect(repos, names(mapping))])
}

EXTS_IN_RELEASE_IMAGE <- repos_to_classes(fetch_image_extensions("extensions-v8-release.yaml"))
EXTS_IN_UCLV2_IMAGE <- repos_to_classes(fetch_image_extensions("extensions-v8-UCL2-release.yaml"))

IMAGES <- list(
  release = list(
    image = "ghcr.io/landis-ii-foundation/landis-ii-v8-release:main",
    extensions = EXTS_IN_RELEASE_IMAGE
  ),
  uclv2 = list(
    image = "ghcr.io/landis-ii-foundation/landis-ii-v8-uclv2-release:main",
    extensions = EXTS_IN_UCLV2_IMAGE
  )
)

filter_extensions_for_image <- function(exts, allowed_classes) {
  keep <- vapply(exts, function(e) class(e)[1] %in% allowed_classes, logical(1))
  exts[keep]
}

##' Download a directory subtree from a GitHub repo's tarball, flattening
##' all files into `dest_dir` (no subdirectories preserved). Caches the
##' tarball under tempdir() so multiple scenarios share one download.
download_repo_subtree <- function(repo, ref, subtree, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- file.path(tempdir(), "landisutils-tarballs")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  tarball <- file.path(cache_dir, sprintf("%s-%s.tar.gz", gsub("/", "_", repo), ref))
  if (!file.exists(tarball)) {
    url <- sprintf("https://codeload.github.com/%s/tar.gz/%s", repo, ref)
    message(sprintf("  fetching tarball: %s", url))
    rc <- utils::download.file(url, tarball, mode = "wb", quiet = TRUE)
    if (rc != 0L || !file.exists(tarball)) {
      stop(sprintf("failed to download %s", url), call. = FALSE)
    }
  }
  ## tar root is "<repo-basename>-<ref>"; subtree paths inside the tarball
  ## are "<root>/<subtree>/...".
  all <- utils::untar(tarball, list = TRUE)
  prefix <- sprintf("[^/]+/%s/", subtree)
  matches <- grep(prefix, all, value = TRUE)
  matches <- matches[!grepl("/$", matches)] ## drop directory entries
  if (length(matches) == 0L) {
    stop(sprintf("no files matched subtree '%s' in %s@%s", subtree, repo, ref), call. = FALSE)
  }
  staging <- tempfile("subtree_")
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  utils::untar(tarball, files = matches, exdir = staging)
  ## Flatten: copy every file's basename to dest_dir.
  src_files <- list.files(staging, recursive = TRUE, full.names = TRUE)
  for (sf in src_files) {
    file.copy(sf, file.path(dest_dir, basename(sf)), overwrite = TRUE)
  }
  invisible(length(src_files))
}

##' Rewrite `(./)?inputs/<subdir>(/<subdir>)*/<filename>` references in every
##' .txt file under <scen_dir> to a flat `./<filename>` form. The upstream
##' Tool-Docker-Apptainer fixtures hardcode subdirectory paths into config
##' files (e.g. EcoregionParameters.txt -> "./inputs/core/climate.txt"); since
##' download_repo_subtree() flattens the input tree, those references must be
##' rewritten or the LANDIS-II runtime fails when opening referenced files.
flatten_path_refs <- function(scen_dir) {
  txt_files <- list.files(scen_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  for (f in txt_files) {
    lines <- readLines(f, warn = FALSE)
    ## Use byte-level regex so files with stray non-UTF-8 bytes don't crash gsub.
    ## Match inputs/<dir>/<dir>/.../<filename> and rewrite to ./<filename>.
    ## Greedy `(?:<seg>/)*` consumes intermediate directories so the final
    ## capture group is the filename only.
    new_lines <- gsub(
      "(\\./)?inputs/(?:[^/[:space:]\"']+/)*([^/[:space:]\"']+)",
      "./\\2",
      lines,
      useBytes = TRUE,
      perl = TRUE
    )
    if (!identical(new_lines, lines)) writeLines(new_lines, f)
  }
}

##' Generate one (scenario, image) pair under <out_dir>/<scen_name>__<image_id>/.
##' Returns the scenario directory path on success, or NULL when the scenario
##' is skipped because a required extension (typically the succession backend)
##' is missing from the target image.
build_one <- function(scen_name, image_id, image_info, builder, out_dir) {
  scen_dir <- file.path(out_dir, sprintf("%s__%s", scen_name, image_id))
  if (dir.exists(scen_dir)) {
    unlink(scen_dir, recursive = TRUE)
  }
  dir.create(scen_dir, recursive = TRUE)
  message(sprintf("Building scenario: %s on %s", scen_name, image_id))
  errored <- FALSE
  result <- tryCatch(builder(scen_dir, image_info$extensions), error = function(e) {
    message(sprintf("  ERROR building %s on %s: %s", scen_name, image_id, conditionMessage(e)))
    ## Leave the partial scen_dir on disk so the failure can be inspected.
    writeLines(conditionMessage(e), file.path(scen_dir, ".build_error"))
    errored <<- TRUE
    NULL
  })
  if (is.null(result)) {
    if (!errored) {
      ## Intentional skip (e.g. required succession backend missing from
      ## image): clean up the empty scenario directory.
      unlink(scen_dir, recursive = TRUE)
      message(sprintf("  skipping %s on %s (incompatible with this image)", scen_name, image_id))
    }
    return(invisible(NULL))
  }
  writeLines(image_info$image, file.path(scen_dir, ".landis_image"))
  scen_dir
}

## ---------------------------------------------------------------------------
## Scenario: NECN AllExtension
##
## Mirrors `tests/TestNECN_UCLv2_AllExtension/scenario.txt` from the upstream
## Tool-Docker-Apptainer repo. NECN Succession + 9 disturbance/output
## extensions, all UCL v2-compatible -- the scenario therefore runs unfiltered
## on both Docker images.
##
## Layout: flat (every file in <scen_dir>). The package regenerates each
## extension's config file on top of upstream-provided ancillary inputs
## (rasters, CSVs, climate). Parameter values mostly mirror our existing
## `tests/testthat/test-ext_*.R` Core8 fixtures, which already produce
## parser-compatible output.
## ---------------------------------------------------------------------------

build_necn_all_extension <- function(scen_dir, allowed_classes) {
  ## Download upstream NECN test inputs (flattened into scen_dir).
  download_repo_subtree(TDA_REPO, TDA_REF, "tests/TestNECN_UCLv2_AllExtension/inputs", scen_dir)
  flatten_path_refs(scen_dir)

  scen_name <- basename(scen_dir)

  ## ---- NECN Succession ----------------------------------------------------
  ## Soil-map paths must be absolute when passed to NECNSuccession$new(); the
  ## active binding's .relPath() interprets bare filenames as CWD-relative.
  abs <- function(name) file.path(scen_dir, name)
  soil_map_names <- list(
    SoilDepthMapName = abs("random110.tif"),
    SoilDrainMapName = abs("constantpoint75.tif"),
    SoilBaseFlowMapName = abs("constantpoint4.tif"),
    SoilStormFlowMapName = abs("constantpoint4.tif"),
    SoilFieldCapacityMapName = abs("random_point1_point2.tif"),
    SoilWiltingPointMapName = abs("random_point05_point099.tif"),
    SoilPercentClayMapName = abs("random_point01_point5.tif"),
    SoilPercentSandMapName = abs("random_point01_point5.tif"),
    InitialSOM1CsurfMapName = abs("random110.tif"),
    InitialSOM1NsurfMapName = abs("random6.tif"),
    InitialSOM1CsoilMapName = abs("random110.tif"),
    InitialSOM1NsoilMapName = abs("random9.tif"),
    InitialSOM2CMapName = abs("random4500.tif"),
    InitialSOM2NMapName = abs("random145.tif"),
    InitialSOM3CMapName = abs("random1294.tif"),
    InitialSOM3NMapName = abs("random50.tif"),
    InitialDeadWoodSurfaceMapName = abs("random110.tif"),
    InitialDeadCoarseRootsMapName = abs("random50.tif")
  )
  fire_params <- tibble::tribble(
    ~FireSeverity        , ~CoarseDebrisReduction , ~FineLitterReduction ,
    ~CohortWoodReduction , ~CohortLeafReduction   , ~SOMReduction        ,
    1L                   , 0.0                    , 0.5                  , 0.05 , 0.85 , 0.10 ,
    2L                   , 0.5                    , 0.75                 , 0.15 , 0.95 , 0.50 ,
    3L                   , 1.0                    , 1.0                  , 0.35 , 1.00 , 0.75
  )
  harvest_params <- tibble::tribble(
    ~PrescriptionName  , ~DeadWoodReduction , ~DeadLitterReduction , ~SOMReduction ,
    ~CohortWoodRemoval , ~CohortLeafRemoval ,
    "MaxAgeClearcut"   , 0.5                , 0.15                 , 0.2           , 0.8 , 0.15 ,
    "PatchCutting"     , 1.0                , 1.0                  , 1.0           , 1.0 , 1.0
  )
  ext_necn <- NECNSuccession$new(
    path = scen_dir,
    Timestep = 5L,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesCSV = file.path(scen_dir, "initial-communities.csv"),
    InitialCommunitiesMap = file.path(scen_dir, "initial-communities.img"),
    ClimateConfigFile = file.path(scen_dir, "climate-generator-baseline.txt"),
    SoilMaps = soil_map_names,
    CalibrateMode = FALSE,
    SmokeModelOutputs = FALSE,
    WaterDecayFunction = "Ratio",
    ProbabilityEstablishAdjust = 1.0,
    InitialMineralN = 2.0,
    InitialFineFuels = 0.99,
    AtmosphericNSlope = 0.05,
    AtmosphericNIntercept = 0.05,
    Latitude = 44.0,
    DenitrificationRate = 0.001,
    DecayRateSurf = 0.01,
    DecayRateSOM1 = 0.01,
    DecayRateSOM2 = 0.2,
    DecayRateSOM3 = 0.001,
    SpeciesParameters = file.path(scen_dir, "NECN_Spp_Table.csv"),
    FireReductionParameters = fire_params,
    HarvestReductionParameters = harvest_params
  )

  ## ---- Social Climate Fire (SCRAPPLE) ------------------------------------
  scrpple_species <- tibble::tribble(
    ~SpeciesCode , ~AgeDBH , ~MaximumBarkThickness ,
    "abiebals"   ,     100 ,                    10 , "acerrubr" , 100 , 10 , "acersacc" , 100 , 10 ,
    "betualle"   ,     100 ,                    10 , "fraxamer" , 100 , 10 , "piceglau" , 100 , 10 ,
    "pinubank"   ,     100 ,                    10 , "pinuresi" , 100 , 10 , "pinustro" , 100 , 10 ,
    "poputrem"   ,     100 ,                    10 , "querelli" , 100 , 10 , "querrubr" , 100 , 10 ,
    "thujocci"   ,     100 ,                    10 , "tiliamer" , 100 , 10
  )
  scrpple_species_file <- prepSpeciesData(
    df = scrpple_species,
    type = "fire",
    path = scen_dir,
    filename = "SCRPPLE_Spp_Table.csv"
  )
  scrpple_supp <- tibble::tribble(
    ~IgnitionType        , ~Mapcode     , ~Suppress_Category_0 , ~FWI_Break_1 ,
    ~Suppress_Category_1 , ~FWI_Break_2 , ~Suppress_Category_2 ,
    "Accidental"         ,            1 ,                   10 ,           20 , 20 , 30 , 0 ,
    "Lightning"          ,            1 ,                   10 ,           20 , 20 , 30 , 0 ,
    "Rx"                 ,            1 ,                   10 ,           20 , 20 , 30 , 0
  )
  scrpple_supp_file <- prepSuppression_CSV_File(
    scrpple_supp,
    path = scen_dir,
    filename = "Suppression_Input.csv"
  )
  ## SCRAPPLE expects rasters; reuse the random*.tif fixtures (any extant raster
  ## file path lets the parser proceed -- ecologically meaningless).
  rast <- function(name) file.path(scen_dir, name)
  ext_scf <- SocialClimateFire$new(
    path = scen_dir,
    Timestep = 1L,
    Species_CSV_File = scrpple_species_file,
    AccidentalIgnitionsMap = rast("Accidental_Ignition_Map.tif"),
    LightningIgnitionsMap = rast("Lightning_Ignition_Map.tif"),
    RxIgnitionsMap = rast("Lightning_Ignition_Map.tif"),
    AccidentalSuppressionMap = rast("Suppression_3Zones.tif"),
    LightningSuppressionMap = rast("Suppression_3Zones.tif"),
    RxSuppressionMap = rast("Suppression_3Zones.tif"),
    GroundSlopeMap = rast("GroundSlope.tif"),
    UphillSlopeAzimuthMap = rast("UphillSlope.tif"),
    ClayMap = rast("random_point01_point5.tif"),
    LightningIgnitionsCoeffs = c(-8.5, 0.03),
    AccidentalIgnitionsCoeffs = c(-8.5, 0.03),
    IgnitionDistribution = "ZeroInflatedPoisson",
    LightningIgnitionsBinomialCoeffs = c(-8.5, 0.03),
    AccidentalIgnitionsBinomialCoeffs = c(-8.5, 0.03),
    MaximumFineFuels = 500.0,
    MaximumRxWindSpeed = 80.0,
    MaximumRxFireWeatherIndex = 80.0,
    MinimumRxFireWeatherIndex = 1.0,
    MaximumRxTemperature = 35.0,
    MinimumRxRelativeHumidity = 22.0,
    MaximumRxFireIntensity = 1,
    NumberRxAnnualFires = 10,
    NumberRxDailyFires = 1,
    FirstDayRxFires = 2,
    LastDayRxFires = 300,
    TargetRxSize = 40,
    MaximumSpreadAreaCoeffs = c(10, -2.5, -2.5),
    SpreadProbabilityCoeffs = c(-1.79, 0.06, -0.915, 0.0126),
    SiteMortalityCoeffs = c(0.0059, 0.00050, -0.000010, -0.0002200, -0.00000050, 0.00000, 0.00000),
    CohortMortalityCoeffs = c(-0.703, -0.9908, 0.009),
    LadderFuelMaxAge = 40,
    LadderFuelSpeciesList = c("abiebals", "pinubank"),
    SuppressionMaxWindSpeed = 100,
    Suppression_CSV_File = scrpple_supp_file,
    DeadWoodTable = data.frame(species = c("pinubank"), age = 22)
  )

  ## ---- Biomass Harvest ----------------------------------------------------
  ## Skeletal management/stands rasters are supplied upstream; reuse those
  ## that exist in the flattened dir.
  rx_clearcut <- harvestPrescription(
    name = "MaxAgeClearcut",
    StandRanking = "MaxCohortAge",
    SiteSelection = "Complete",
    CohortsRemoved = "ClearCut",
    MultipleRepeat = 20L
  )
  bh_impl <- tibble::tribble(
    ~MgmtArea , ~Prescription    , ~HarvestArea , ~BeginTime , ~EndTime ,
    1L        , "MaxAgeClearcut" , "10%"        , 5L         , 50L
  )
  ext_bh <- BiomassHarvest$new(
    path = scen_dir,
    Timestep = 5L,
    ManagementAreas = file.path(scen_dir, "harvest_Management_zones.tif"),
    Stands = file.path(scen_dir, "harvest_Stands.tif"),
    Prescriptions = list(rx_clearcut),
    HarvestImplementations = bh_impl,
    PrescriptionMaps = "harvest/biomass-harvest-prescripts-{timestep}.tif",
    BiomassMaps = "harvest/biomass-removed-{timestep}.tif",
    EventLog = file.path(scen_dir, "harvest/biomass-harvest-event-log.csv"),
    SummaryLog = file.path(scen_dir, "harvest/biomass-harvest-summary-log.csv")
  )

  ## ---- Original Wind ------------------------------------------------------
  ow_wep <- tibble::tribble(
    ~Ecoregion , ~MaxSize , ~MeanSize , ~MinSize , ~WindRotationPeriod ,
    "eco1"     ,      400 ,       100 ,        4 , 100L
  )
  ext_ow <- OriginalWind$new(
    path = scen_dir,
    Timestep = 5L,
    WindEventParametersTable = ow_wep,
    MapNames = "wind/severity-{timestep}.tif",
    SummaryLogFile = file.path(scen_dir, "wind/wind-summary-log.csv"),
    EventLogFile = file.path(scen_dir, "wind/wind-events-log.csv")
  )

  ## ---- Hurricane ----------------------------------------------------------
  hu_occ <- data.frame(
    Storms = 0:8,
    Probability = c(0.05, 0.15, 0.22, 0.22, 0.17, 0.10, 0.05, 0.03, 0.01)
  )
  hu_exp <- data.frame(
    Degree = c(135L, 180L, 225L),
    MapName = c("test_135_wind_fix.tif", "test_180_wind_fix.tif", "test_225_wind_fix.tif")
  )
  hu_curve <- defaultHurricaneMortalityCurve()
  hu_vulns <- lapply(c("abiebals", "pinubank"), function(sp) {
    windSpeedVulnerability(species = sp, maxAge = 800, mortality = hu_curve)
  })
  ext_hu <- Hurricane$new(
    path = scen_dir,
    Timestep = 1L,
    InputUnitsEnglish = FALSE,
    HurricaneRandomNumberSeed = 1974L,
    StormOccurrenceProbabilities = hu_occ,
    LowBoundLandfallWindSpeed = 51,
    ModeLandfallWindSpeed = 165,
    HighBoundLandfallWindSpeed = 161,
    CoastalSlope = -0.903,
    MeanStormIntersectionX = 417.618,
    MeanStormIntersectionY = 105.420,
    LandfallSigma = 1,
    StormDirectionMu = 204.4397,
    StormDirectionSigma = 71.06188,
    MinimumWindSpeedforDamage = 60,
    ExposureMaps = hu_exp,
    WindSpeedVulnerabilities = hu_vulns,
    LogFile = "hurricane-log.csv",
    WindReductionTableCSV = file.path(scen_dir, "EvennessWindReductions.csv")
  )

  ## ---- Output extensions --------------------------------------------------
  ext_omsa <- OutputMaxSpeciesAge$new(path = scen_dir, Timestep = 10, Species = "all")
  ext_obc <- OutputBiomassCommunity$new(path = scen_dir, Timestep = 10L)
  ext_ob <- OutputBiomass$new(path = scen_dir, Timestep = 10, Species = "all")
  reclass_maps <- list(
    forest = list(
      Conifers = c("abiebals", "piceglau", "pinubank", "pinustro", "thujocci"),
      Hardwoods = c("acerrubr", "acersacc", "betualle", "fraxamer", "poputrem"),
      Other = c("querelli", "querrubr", "tiliamer")
    )
  )
  ext_obr <- OutputBiomassReclass$new(
    path = scen_dir,
    Timestep = 10L,
    ReclassMaps = reclass_maps,
    MapFileNames = "outputs/biomass-reclass/{reclass-map-name}-{timestep}.tif"
  )
  ext_ocs <- OutputCohortStats$new(
    path = scen_dir,
    Timestep = 10,
    SpeciesAgeStats = list(species = c("pinubank"), stats = c("MAX", "MIN")),
    SiteAgeStats = list(stats = c("MAX", "MED")),
    SiteSpeciesStats = list(stats = c("RICH"))
  )

  ## ---- Filter, write, and assemble scenario.txt --------------------------
  exts <- list(
    ext_necn,
    ext_scf,
    ext_bh,
    ext_ow,
    ext_hu,
    ext_omsa,
    ext_obc,
    ext_ob,
    ext_obr,
    ext_ocs
  )
  exts <- filter_extensions_for_image(exts, allowed_classes)

  if (!any(vapply(exts, function(e) e$type == "succession", logical(1)))) {
    return(NULL)
  }

  for (e in exts) {
    e$write()
  }

  climate_cfg <- LandisClimateConfig$new(path = scen_dir)
  climate_cfg$add_file("climate.txt")

  scen <- scenario(
    name = scen_name,
    extensions = exts,
    climate_config = climate_cfg,
    path = scen_dir,
    Duration = 50,
    EcoregionsFiles = c(file.path(scen_dir, "ecoregion.txt"), file.path(scen_dir, "ecoregion.img")),
    SpeciesInputFile = file.path(scen_dir, "species.txt"),
    CellLength = 30,
    DisturbancesRandomOrder = FALSE,
    RandomNumberSeed = 1111
  )

  scen_dir
}

## ---------------------------------------------------------------------------
## Scenario: PnET AllExtension
##
## Mirrors `tests/TestPnET_AllExtension/scenario.txt`. PnET-Succession is NOT
## registered in the UCL v2 image, so the (PnET, uclv2) pair is skipped.
## Several other extensions in this scenario (LandUsePlus, ClimateBDA,
## DynamicFire/Fuels, LinearWind, OutputBiomassPnET, OutputLocal/Wildlife
## Habitat) are also UCL-v1-only, so even if a PnET-equivalent succession
## existed in UCL v2, those extensions would be filtered out individually.
## ---------------------------------------------------------------------------

build_pnet_all_extension <- function(scen_dir, allowed_classes) {
  download_repo_subtree(TDA_REPO, TDA_REF, "tests/TestPnET_AllExtension/inputs", scen_dir)
  flatten_path_refs(scen_dir)

  scen_name <- basename(scen_dir)

  ## Bail out fast if PnET-Succession isn't registered in this image.
  if (!"PnETSuccession" %in% allowed_classes) {
    return(NULL)
  }

  ## ---- PnET-Succession ---------------------------------------------------
  ## Upstream provides EcoregionParameters.txt, SpeciesParameters.txt,
  ## PnETGenericParameters.txt, disturbance_reductions.txt -- reference
  ## those verbatim from the PnETSuccession config.
  ext_pnet <- PnETSuccession$new(
    path = scen_dir,
    Timestep = 1L,
    StartYear = 2000L,
    SeedingAlgorithm = "WardSeedDispersal",
    Latitude = 44,
    InitialCommunities = file.path(scen_dir, "initial-communities.csv"),
    InitialCommunitiesMap = file.path(scen_dir, "initial-communities.img"),
    PnETGenericParameters = file.path(scen_dir, "PnETGenericParameters.txt"),
    PnETSpeciesParameters = file.path(scen_dir, "SpeciesParameters.txt"),
    EcoregionParameters = file.path(scen_dir, "EcoregionParameters.txt")
  )

  ## ---- Original Fire -----------------------------------------------------
  of_species <- tibble::tribble(
    ~SpeciesCode , ~AgeDBH , ~MaximumBarkThickness ,
    "abiebals"   ,     100 ,                    10 , "acerrubr" , 100 , 10 , "acersacc" , 100 , 10 ,
    "betualle"   ,     100 ,                    10 , "fraxamer" , 100 , 10 , "piceglau" , 100 , 10 ,
    "pinubank"   ,     100 ,                    10 , "pinuresi" , 100 , 10 , "pinustro" , 100 , 10 ,
    "poputrem"   ,     100 ,                    10 , "querelli" , 100 , 10 , "querrubr" , 100 , 10 ,
    "thujocci"   ,     100 ,                    10 , "tiliamer" , 100 , 10
  )
  ## Original Fire's species CSV format is just SpeciesCode + FireTolerance,
  ## different from the SCRAPPLE/Dynamic Fire format. Use the upstream-shipped
  ## file directly rather than regenerating via prepSpeciesData().
  of_species_file <- file.path(scen_dir, "OriginalFire_Spp_Table.csv")
  ## Original Fire's parser reads exactly 7 positional columns:
  ## Region, MapCode, Mean, Min, Max, IgnitionProb, k. The package's
  ## insertFireRegionParametersTable() writes every column it's given, so
  ## include only the seven the parser expects, in that exact order.
  of_frp <- data.frame(
    FireRegionName = "fire1",
    MapCode = 1L,
    MeanSize = 100,
    MinSize = 4,
    MaxSize = 400,
    IgnitionProb = 0.001,
    K = 100
  )
  ext_of <- OriginalFire$new(
    path = scen_dir,
    Timestep = 10L,
    Species_CSV_File = of_species_file,
    FireRegionParametersTable = of_frp,
    InitialFireRegionsMap = file.path(scen_dir, "ecoregion.img"),
    FuelCurveTable = defaultFuelCurveTable(of_frp),
    WindCurveTable = NULL,
    FireDamageTable = defaultFireDamageTable(),
    MapNames = "fire/severity-{timestep}.tif",
    LogFile = "fire/log.csv",
    SummaryLogFile = "fire/summary-log.csv"
  )

  ## ---- Social Climate Fire (same skeleton as NECN scenario) -------------
  scrpple_species_file <- prepSpeciesData(
    of_species,
    type = "fire",
    path = scen_dir,
    filename = "SCRPPLE_Spp_Table.csv"
  )
  scrpple_supp <- tibble::tribble(
    ~IgnitionType        , ~Mapcode     , ~Suppress_Category_0 , ~FWI_Break_1 ,
    ~Suppress_Category_1 , ~FWI_Break_2 , ~Suppress_Category_2 ,
    "Accidental"         ,            1 ,                   10 ,           20 , 20 , 30 , 0 ,
    "Lightning"          ,            1 ,                   10 ,           20 , 20 , 30 , 0 ,
    "Rx"                 ,            1 ,                   10 ,           20 , 20 , 30 , 0
  )
  scrpple_supp_file <- prepSuppression_CSV_File(
    scrpple_supp,
    path = scen_dir,
    filename = "Suppression_Input.csv"
  )
  rast_pnet <- function(name) file.path(scen_dir, name)
  ext_scf <- SocialClimateFire$new(
    path = scen_dir,
    Timestep = 1L,
    Species_CSV_File = scrpple_species_file,
    AccidentalIgnitionsMap = rast_pnet("ecoregion.img"),
    LightningIgnitionsMap = rast_pnet("ecoregion.img"),
    RxIgnitionsMap = rast_pnet("ecoregion.img"),
    AccidentalSuppressionMap = rast_pnet("ecoregion.img"),
    LightningSuppressionMap = rast_pnet("ecoregion.img"),
    RxSuppressionMap = rast_pnet("ecoregion.img"),
    GroundSlopeMap = rast_pnet("ecoregion.img"),
    UphillSlopeAzimuthMap = rast_pnet("ecoregion.img"),
    ClayMap = rast_pnet("ecoregion.img"),
    LightningIgnitionsCoeffs = c(-8.5, 0.03),
    AccidentalIgnitionsCoeffs = c(-8.5, 0.03),
    IgnitionDistribution = "ZeroInflatedPoisson",
    LightningIgnitionsBinomialCoeffs = c(-8.5, 0.03),
    AccidentalIgnitionsBinomialCoeffs = c(-8.5, 0.03),
    MaximumFineFuels = 500.0,
    MaximumRxWindSpeed = 80.0,
    MaximumRxFireIntensity = 1,
    NumberRxAnnualFires = 10,
    NumberRxDailyFires = 1,
    FirstDayRxFires = 2,
    LastDayRxFires = 300,
    TargetRxSize = 40,
    MaximumSpreadAreaCoeffs = c(10, -2.5, -2.5),
    SpreadProbabilityCoeffs = c(-1.79, 0.06, -0.915, 0.0126),
    SiteMortalityCoeffs = c(0.0059, 0.00050, -0.000010, -0.0002200, -0.00000050, 0.00000, 0.00000),
    CohortMortalityCoeffs = c(-0.703, -0.9908, 0.009),
    LadderFuelMaxAge = 40,
    LadderFuelSpeciesList = c("abiebals"),
    SuppressionMaxWindSpeed = 100,
    Suppression_CSV_File = scrpple_supp_file,
    DeadWoodTable = data.frame(species = "pinubank", age = 22)
  )

  ## ---- Dynamic Fire System & Fuels ---------------------------------------
  df_sizes <- tibble::tribble(
    ~EcoCode       , ~EcoName   , ~Mu         , ~Sigma , ~Max ,
    ~SpFMCLo       , ~SpFMCHi   , ~SpHiProp   ,
    ~SumFMCLo      , ~SumFMCHi  , ~SumHiProp  ,
    ~FallFMCLo     , ~FallFMCHi , ~FallHiProp ,
    ~OpenFuelIndex , ~NumFires  ,
                 1 , "fire1"    , 4.0         ,  0.1   ,   50 ,
                85 ,        100 , 0.50        , 92     ,  120 , 0.50 , 120 , 120 , 0.50 , 2 , 0.5
  )
  df_season <- tibble::tribble(
    ~Name    , ~LeafStatus , ~PropFire , ~PercentCuring , ~DayLengthProp ,
    "Spring" , "LeafOff"   , 0.20      ,             50 , 1.0            ,
    "Summer" , "LeafOn"    , 0.50      ,             51 , 1.0            ,
    "Fall"   , "LeafOff"   , 0.30      ,            100 , 1.0
  )
  ext_df <- DynamicFire$new(
    path = scen_dir,
    Timestep = 10,
    Species_CSV_File = file.path(scen_dir, "DynamicFire_Spp_Table.csv"),
    EventSizeType = "size_based",
    BuildUpIndex = "yes",
    WeatherRandomizer = 0L,
    FireSizesTable = df_sizes,
    InitialFireEcoregionsMap = file.path(scen_dir, "ecoregion.img"),
    GroundSlopeFile = file.path(scen_dir, "ecoregion.img"),
    UphillSlopeAzimuthMap = file.path(scen_dir, "ecoregion.img"),
    SeasonTable = df_season,
    InitialWeatherDatabase = file.path(scen_dir, "dynamic-fire_WeatherData.csv"),
    FuelTypeTable = defaultFuelTypeTable(),
    SeverityCalibrationFactor = 1.0,
    FireDamageTable = defaultFireDamageTable(),
    LogFile = file.path(scen_dir, "fire/dynamic-fire-event-log.csv"),
    SummaryLogFile = file.path(scen_dir, "fire/dynamic-fire-summary-log.csv")
  )
  duf_spp_coeffs <- tibble::tribble(
    ~Species   , ~FuelCoefficient ,
    "abiebals" , 1.00             , "acerrubr" , 0.50 , "acersacc" , 1.00 ,
    "betualle" , 1.00             , "fraxamer" , 1.00 , "piceglau" , 1.00 ,
    "pinubank" , 1.00             , "pinuresi" , 1.00 , "pinustro" , 1.00 ,
    "poputrem" , 1.00             , "querelli" , 1.00 , "querrubr" , 1.00 ,
    "thujocci" , 1.00             , "tiliamer" , 1.00
  )
  duf_fuel_types <- tibble::tribble(
    ~FuelType , ~BaseFuel   , ~AgeMin , ~AgeMax , ~Species                                                                                             ,
            1 , "Conifer"   ,       0 ,     900 , list("thujocci")                                                                                     ,
            2 , "Conifer"   ,       0 ,     500 , list("piceglau", "abiebals")                                                                         ,
            8 , "Deciduous" ,       0 ,     300 , list("acerrubr", "acersacc", "betualle", "fraxamer", "poputrem", "querelli", "querrubr", "tiliamer")
  )
  duf_disturb_conv <- tibble::tribble(
    ~Fuel , ~Type , ~Duration       , ~Prescription    ,
       14 ,    20 , "WindSeverity3" , "MaxAgeClearcut"
  )
  ext_duf <- DynamicFuels$new(
    path = scen_dir,
    Timestep = 10,
    SpeciesFuelCoefficients = duf_spp_coeffs,
    HardwoodMaximum = 15L,
    DeadFirMaxAge = 15L,
    FuelTypes = duf_fuel_types,
    EcoregionTable = data.frame(FuelType = integer(0), Ecoregion = character(0)),
    DisturbanceConversionTable = duf_disturb_conv
  )

  ## ---- Magic Harvest (wraps Biomass Harvest) -----------------------------
  ext_mh <- MagicHarvest$new(
    path = scen_dir,
    Timestep = 10L,
    HarvestExtensionParameterFile = "biomass-harvest_SetUp_s2e1.txt",
    ProcessToLaunch = "echo",
    ProcessArguments = "noop {timestep}",
    NoHarvestReInitialization = TRUE
  )
  ## The wrapped Biomass Harvest config -- Magic Harvest looks for it by name.
  rx_bh <- harvestPrescription(
    name = "MaxAgeClearcut",
    StandRanking = "MaxCohortAge",
    SiteSelection = "Complete",
    CohortsRemoved = "ClearCut",
    MultipleRepeat = 20L
  )
  bh_impl <- tibble::tribble(
    ~MgmtArea , ~Prescription    , ~HarvestArea , ~BeginTime , ~EndTime ,
    1L        , "MaxAgeClearcut" , "10%"        , 5L         , 50L
  )
  ext_bh <- BiomassHarvest$new(
    path = scen_dir,
    Timestep = 5L,
    ManagementAreas = file.path(scen_dir, "ecoregion.img"),
    Stands = file.path(scen_dir, "ecoregion.img"),
    Prescriptions = list(rx_bh),
    HarvestImplementations = bh_impl,
    PrescriptionMaps = "harvest/prescripts-{timestep}.tif",
    BiomassMaps = "harvest/removed-{timestep}.tif",
    EventLog = file.path(scen_dir, "harvest/event-log.csv"),
    SummaryLog = file.path(scen_dir, "harvest/summary-log.csv")
  )
  ## Override: Magic Harvest expects the wrapped Biomass Harvest's config to
  ## live at "biomass-harvest_SetUp_s2e1.txt" (the value referenced above).
  ext_bh$files <- "biomass-harvest_SetUp_s2e1.txt"

  ## ---- Forest Roads Simulation -------------------------------------------
  fr_coarse <- list(
    elevationCostRange(0, 9, 0),
    elevationCostRange(9, 16, 1000),
    elevationCostRange(16, 100, 10000)
  )
  fr_road_types <- list(
    roadType(
      id = 3L,
      name = "Tertiary",
      costMultiplier = 1.0,
      fluxMin = 0,
      fluxMax = 500,
      maxAge = 20L
    ),
    roadType(
      id = 1L,
      name = "Primary",
      costMultiplier = 5.0,
      fluxMin = 500,
      fluxMax = 100000,
      maxAge = 80L
    )
  )
  fr_exit <- list(
    exitRoadType(id = 5L, name = "Sawmill"),
    exitRoadType(id = 6L, name = "MainRoadNetworkPaved")
  )
  ext_fr <- ForestRoadsSimulation$new(
    path = scen_dir,
    Timestep = 10L,
    HeuristicForNetworkConstruction = "ClosestFirst",
    SkiddingDistance = 150,
    LoopingBehavior = FALSE,
    RasterOfBuildableZones = "soils.tif",
    InitialRoadNetworkMap = "roads.tif",
    DistanceCost = 10000,
    CoarseElevationRaster = "coarse_elevation.tif",
    CoarseElevationCosts = fr_coarse,
    SoilsRaster = NULL,
    SimulationOfRoadAging = TRUE,
    SimulationOfWoodFlux = TRUE,
    RoadTypes = fr_road_types,
    RoadTypesForExitingWood = fr_exit
  )

  ## ---- Original Wind ------------------------------------------------------
  ow_wep <- tibble::tribble(
    ~Ecoregion , ~MaxSize , ~MeanSize , ~MinSize , ~WindRotationPeriod ,
    "eco1"     ,      400 ,       100 ,        4 , 100L
  )
  ext_ow <- OriginalWind$new(
    path = scen_dir,
    Timestep = 5L,
    WindEventParametersTable = ow_wep,
    MapNames = "wind/severity-{timestep}.tif",
    SummaryLogFile = file.path(scen_dir, "wind/wind-summary-log.csv"),
    EventLogFile = file.path(scen_dir, "wind/wind-events-log.csv")
  )

  ## ---- Linear Wind --------------------------------------------------------
  ext_lw <- LinearWind$new(
    path = scen_dir,
    Timestep = 10L,
    NumEventsMean = 8.4,
    NumEventsStDev = 0.08,
    TornadoLengthLambda = 25.75,
    TornadoLengthAlpha = 26.5,
    TornadoWidth = 0.420,
    TornadoIntensityTable = c(0, 5, 20, 50, 25),
    TornadoProp = 0.76,
    DerechoLengthLambda = 178.0,
    DerechoLengthAlpha = 3.6,
    DerechoWidth = 40.000,
    DerechoIntensityTable = c(5, 15, 50, 25, 5),
    PropIntensityVar = 0.65,
    WindDirectionTable = c(0, 30, 60, 10),
    EcoregionModifiers = data.frame(Ecoregion = "eco1", Modifier = -0.5),
    IntensityMapNames = "linearwind/intensity-{timestep}.tif",
    SeverityMapNames = "linearwind/severity-{timestep}.tif",
    LogFile = file.path(scen_dir, "linearwind/event-log.csv")
  )

  ## ---- Hurricane ----------------------------------------------------------
  hu_occ <- data.frame(
    Storms = 0:8,
    Probability = c(0.05, 0.15, 0.22, 0.22, 0.17, 0.10, 0.05, 0.03, 0.01)
  )
  hu_exp <- data.frame(
    Degree = c(135L, 180L, 225L),
    MapName = c("ecoregion.img", "ecoregion.img", "ecoregion.img")
  )
  hu_curve <- defaultHurricaneMortalityCurve()
  hu_vulns <- lapply(c("abiebals", "pinubank"), function(sp) {
    windSpeedVulnerability(species = sp, maxAge = 800, mortality = hu_curve)
  })
  ext_hu <- Hurricane$new(
    path = scen_dir,
    Timestep = 1L,
    InputUnitsEnglish = FALSE,
    HurricaneRandomNumberSeed = 1974L,
    StormOccurrenceProbabilities = hu_occ,
    LowBoundLandfallWindSpeed = 51,
    ModeLandfallWindSpeed = 165,
    HighBoundLandfallWindSpeed = 161,
    CoastalSlope = -0.903,
    MeanStormIntersectionX = 417.618,
    MeanStormIntersectionY = 105.420,
    LandfallSigma = 1,
    StormDirectionMu = 204.4397,
    StormDirectionSigma = 71.06188,
    MinimumWindSpeedforDamage = 60,
    ExposureMaps = hu_exp,
    WindSpeedVulnerabilities = hu_vulns,
    LogFile = "hurricane-log.csv",
    WindReductionTableCSV = file.path(scen_dir, "EvennessWindReductions.csv")
  )

  ## ---- Climate BDA --------------------------------------------------------
  bda_sp <- tibble::tribble(
    ~Species       , ~MinorHostAge      , ~MinorHostSRDProb ,
    ~SecondHostAge , ~SecondHostSRDProb ,
    ~MajorHostAge  , ~MajorHostSRDProb  ,
    ~Class3Age     , ~Class3VulnProb    ,
    ~Class2Age     , ~Class2VulnProb    ,
    ~Class1Age     , ~Class1VulnProb    , ~CFSConifer       ,
    "abiebals"     ,                  0 , 0.25              , 20 , 0.5 , 40 , 1.0 , 11 , 1.0 , 20 , 1.00 , 50 , 1.00 , "yes" ,
    "piceglau"     ,                  0 , 0.25              , 20 , 0.5 , 40 , 1.0 ,  0 , 0.0 , 20 , 0.15 , 50 , 0.42 , "yes"
  )
  bda_agent <- bdaAgent(
    name = "budworm",
    BDPCalibrator = 1,
    SRDMode = "mean",
    OutbreakPattern = "CyclicNormal",
    Mean = 33.5,
    StDev = 10.6,
    TimeSinceLastEpidemic = 10L,
    TemporalType = "variablepulse",
    MinROS = 0L,
    MaxROS = 3L,
    Dispersal = "no",
    EpidemicThresh = 0.5,
    InitialEpicenterNum = 0L,
    OutbreakEpicenterCoeff = 0.01,
    OutbreakEpicenterThresh = 0.0,
    SeedEpicenter = "yes",
    SeedEpicenterCoeff = 0.5,
    DispersalTemplate = "MaxRadius",
    NeighborFlag = "yes",
    NeighborSpeedUp = "none",
    NeighborRadius = 150,
    NeighborShape = "uniform",
    NeighborWeight = 100,
    IntensityClass2_BDP = 0.25,
    IntensityClass3_BDP = 0.50,
    BDASpeciesParameters = bda_sp,
    IgnoredSpecies = c("pinubank", "thujocci")
  )
  ext_bda <- ClimateBDA$new(
    path = scen_dir,
    Timestep = 1L,
    Agents = list(bda_agent),
    SRDMapNames = "bda/{agentName}-SRD-{timestep}.tif",
    NRDMapNames = "bda/{agentName}-NRD-{timestep}.tif",
    BDPMapNames = "bda/{agentName}-BDP-{timestep}.tif",
    LogFile = file.path(scen_dir, "bda/bda-log.csv")
  )

  ## ---- Land Use Plus -----------------------------------------------------
  ## Upstream landuse-*.tif files use map codes 1-4; declare a LandUseType
  ## for each so the parser doesn't reject unknown codes.
  forest_lu <- landUseType(
    name = "forest", mapCode = 4L, allowHarvest = "no",
    changes = list(landCoverChange(type = "NoChange"))
  )
  fips_harvest_lu <- landUseType(
    name = "FIPSandHarvest", mapCode = 3L, allowHarvest = "yes",
    changes = list(landCoverChange(type = "NoChange"))
  )
  remove_trees_lu <- landUseType(
    name = "RemoveTreesOnly", mapCode = 2L, allowHarvest = "no",
    changes = list(landCoverChange(type = "NoChange"))
  )
  fips_lu <- landUseType(
    name = "FIPS", mapCode = 1L, allowHarvest = "no",
    changes = list(landCoverChange(type = "NoChange"))
  )
  ## Upstream LandUsePlus inputs ship `landuse-{0,5,10,...,50}.tif` (Timestep=5).
  ext_lup <- LandUsePlus$new(
    path = scen_dir,
    Timestep = 5L,
    InputMaps = "landuse-{timestep}.tif",
    SiteLog = file.path(scen_dir, "land-use/site-log.csv"),
    LandUses = list(forest_lu, fips_harvest_lu, remove_trees_lu, fips_lu)
  )

  ## ---- Output extensions -------------------------------------------------
  ext_omsa <- OutputMaxSpeciesAge$new(path = scen_dir, Timestep = 10, Species = "all")
  ext_obba <- OutputBiomassByAge$new(
    path = scen_dir,
    Timestep = 10,
    Species = c("pinubank ageclass1(10-40)")
  )
  ext_obc <- OutputBiomassCommunity$new(path = scen_dir, Timestep = 10L)
  ext_ob <- OutputBiomass$new(path = scen_dir, Timestep = 10, Species = "all")
  reclass_maps <- list(
    forest = list(
      Conifers = c("abiebals", "piceglau", "pinubank", "pinustro", "thujocci"),
      Hardwoods = c("acerrubr", "acersacc", "betualle", "fraxamer", "poputrem"),
      Other = c("querelli", "querrubr", "tiliamer")
    )
  )
  ext_obr <- OutputBiomassReclass$new(
    path = scen_dir,
    Timestep = 10L,
    ReclassMaps = reclass_maps,
    MapFileNames = "outputs/biomass-reclass/{reclass-map-name}-{timestep}.tif"
  )
  ext_ocs <- OutputCohortStats$new(
    path = scen_dir,
    Timestep = 10,
    SpeciesAgeStats = list(species = c("pinubank"), stats = c("MAX")),
    SiteAgeStats = list(stats = c("MAX")),
    SiteSpeciesStats = list(stats = c("RICH"))
  )
  ## NOTE: keyword `Biomass` appears in the package's `.pnetOutputKeywords`
  ## but the v8-release parser only registers extensions like WoodBiomass,
  ## FoliageBiomass, RootBiomass etc. Use parser-valid keys here.
  ext_obp <- OutputBiomassPnET$new(
    path = scen_dir,
    Timestep = 10L,
    Species = "all",
    Outputs = list(
      WoodBiomass    = "output/WoodBiomass/{species}/WoodBiomass-{timestep}.img",
      FoliageBiomass = "output/FoliageBiomass/{species}/FoliageBiomass-{timestep}.img",
      LeafAreaIndex  = "output/LeafAreaIndex/{species}/LeafAreaIndex-{timestep}.img"
    )
  )
  ext_olh <- OutputLocalHabitat$new(
    path = scen_dir,
    Timestep = 1L,
    OutputTimestep = 10L,
    MapFileNames = "output/habitat/{HabitatName}-{timestep}.tif",
    SuitabilityFiles = c("AgeClass_ForestType_example.txt")
  )
  ext_owh <- OutputWildlifeHabitat$new(
    path = scen_dir,
    Timestep = 1L,
    OutputTimestep = 10L,
    MapFileNames = "output/habitat/{wildlifeName}-{timestep}.tif",
    SuitabilityFiles = c("BBWOAgeForestType.txt")
  )

  ## ---- Filter, write, and assemble scenario.txt --------------------------
  ## Mirror the upstream PnET scenario.txt: Original Fire and Social Climate
  ## Fire are intentionally OMITTED because Dynamic Fire System registers the
  ## same `Fire.Severity` site variable and LANDIS-II disallows two extensions
  ## owning the same variable. Keep at most one fire extension active.
  exts <- list(
    ext_pnet,
    ext_df,
    ext_duf,
    ext_mh,
    ext_bh,
    ext_fr,
    ext_ow,
    ext_lw,
    ext_hu,
    ext_bda,
    ext_lup,
    ext_omsa,
    ext_obba,
    ext_obc,
    ext_ob,
    ext_obr,
    ext_ocs,
    ext_obp,
    ext_olh,
    ext_owh
  )
  exts <- filter_extensions_for_image(exts, allowed_classes)

  if (!any(vapply(exts, function(e) e$type == "succession", logical(1)))) {
    return(NULL)
  }

  for (e in exts) {
    e$write()
  }

  ## Some habitat / suitability ancillary files referenced above don't ship in
  ## the upstream input tree -- create empty stubs so add_file()'s existence
  ## checks pass during scenario assembly.
  for (f in c(
    "AgeClass_ForestType_example.txt",
    "BBWOAgeForestType.txt",
    "land-use/site-log.csv"
  )) {
    full <- file.path(scen_dir, f)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(full)) writeLines("", full)
  }

  climate_cfg <- LandisClimateConfig$new(path = scen_dir)
  ## PnET-Succession uses EcoregionParameters.txt's per-ecoregion ClimateFileName.

  scen <- scenario(
    name = scen_name,
    extensions = exts,
    climate_config = climate_cfg,
    path = scen_dir,
    Duration = 50,
    EcoregionsFiles = c(file.path(scen_dir, "ecoregion.txt"), file.path(scen_dir, "ecoregion.img")),
    SpeciesInputFile = file.path(scen_dir, "species.txt"),
    CellLength = 30,
    DisturbancesRandomOrder = FALSE,
    RandomNumberSeed = 1111
  )

  scen_dir
}

## ---------------------------------------------------------------------------
## Scenario: Biomass Succession + Social Climate Fire + biomass output extensions
##
## More targeted than the AllExtension blocks: validates that Biomass Succession
## config + SCRAPPLE config + every biomass-flavoured output extension parse
## cleanly together. Reuses the same `CoreV8.0-BiomassSuccession7.0` reference
## inputs as the first two scenarios -- SCRAPPLE raster slots all point at
## `ecoregions.tif` since the upstream BiomassSuccession testings dir doesn't
## ship dedicated SCRAPPLE rasters and the parser only needs valid raster
## paths (same approach as the PnET AllExtension scenario).
##
## Skipped on any image that doesn't register both BiomassSuccession and
## SocialClimateFire; individual output extensions get filtered out per image
## by `filter_extensions_for_image()`.
## ---------------------------------------------------------------------------

build_biomass_succession_scrpple <- function(scen_dir, allowed_classes) {
  ## Bail fast: this scenario exists to exercise SCRAPPLE on top of Biomass
  ## Succession, so skip on images that lack either backend.
  if (!all(c("BiomassSuccession", "SocialClimateFire") %in% allowed_classes)) {
    return(NULL)
  }

  ref_base <- paste0(
    "https://raw.githubusercontent.com/",
    "LANDIS-II-Foundation/Extension-Biomass-Succession/master/",
    "testings/CoreV8.0-BiomassSuccession7.0"
  )
  ref_files <- c(
    "Core_species_data.txt",
    "SpeciesData.csv",
    "SppEcoregionData.csv",
    "biomass-succession_ClimateGenerator.txt",
    "biomass-succession_InitialCommunities.csv",
    "ecoregions.tif",
    "ecoregions.txt",
    "initial-communities.tif",
    "PRISM_data_AFRI_4.18.13_v2.csv"
  )
  download_refs(ref_base, scen_dir, ref_files)

  scen_name <- basename(scen_dir)

  ## ---- Biomass Succession (mirrors upstream biomass-succession.txt) ------
  min_rel_b <- tibble::tribble(
    ~ShadeClass , ~Eco1 , ~Eco2 ,
    NA_integer_ , "101" , "102" ,
    1L          , "25%" , "25%" ,
    2L          , "45%" , "45%" ,
    3L          , "56%" , "56%" ,
    4L          , "70%" , "70%" ,
    5L          , "90%" , "90%"
  )
  suff_light <- tibble::tribble(
    ~class , ~X0 , ~X1 , ~X2  , ~X3  , ~X4  , ~X5  ,
    1L     , 1.0 , 0.5 , 0.25 , 0.0  , 0.0  , 0.0  ,
    2L     , 1.0 , 1.0 , 0.5  , 0.25 , 0.0  , 0.0  ,
    3L     , 1.0 , 1.0 , 1.0  , 0.5  , 0.25 , 0.0  ,
    4L     , 1.0 , 1.0 , 1.0  , 1.0  , 0.5  , 0.25 ,
    5L     , 0.1 , 0.5 , 1.0  , 1.0  , 1.0  , 1.0
  )
  erp_df <- tibble::tribble(
    ~ecoregion , ~AET ,
    "101"      ,  600 ,
    "102"      ,  600
  )
  frp_df <- tibble::tribble(
    ~Severity , ~WoodLitterReduct , ~LitterReduct ,
    1L        , 0.0               , 0.5           ,
    2L        , 0.0               , 0.75          ,
    3L        , 0.0               , 1.0
  )
  hrp_df <- tibble::tribble(
    ~Name            , ~WoodLitterReduct , ~LitterReduct , ~CohortWoodRemoval , ~CohortLeafRemoval ,
    "MaxAgeClearcut" , 0.5               , 0.15          , 0.8                , 0.0                ,
    "PatchCutting"   , 1.0               , 1.0           , 1.0                , 0.0
  )
  ext_bs <- BiomassSuccession$new(
    path = scen_dir,
    Timestep = 10,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesFiles = c(
      file.path(scen_dir, "biomass-succession_InitialCommunities.csv"),
      file.path(scen_dir, "initial-communities.tif")
    ),
    ClimateConfigFile = file.path(scen_dir, "biomass-succession_ClimateGenerator.txt"),
    CalibrateMode = NULL,
    SpinupCohorts = FALSE,
    SpinupMortalityFraction = 0.05,
    MinRelativeBiomass = min_rel_b,
    SufficientLight = suff_light,
    SpeciesDataFile = file.path(scen_dir, "SpeciesData.csv"),
    EcoregionParameters = erp_df,
    SpeciesEcoregionDataFile = file.path(scen_dir, "SppEcoregionData.csv"),
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df
  )

  ## ---- Social Climate Fire (SCRAPPLE) ------------------------------------
  ## Species list mirrors Core_species_data.txt; raster slots all point at
  ## `ecoregions.tif` (parser-valid; runtime values are placeholders).
  scrpple_species <- tibble::tribble(
    ~SpeciesCode , ~AgeDBH , ~MaximumBarkThickness ,
    "abiebals"   ,     100 ,                    10 , "acerrubr" , 100 , 10 , "acersacc" , 100 , 10 ,
    "betualle"   ,     100 ,                    10 , "fraxamer" , 100 , 10 , "piceglau" , 100 , 10 ,
    "pinubank"   ,     100 ,                    10 , "pinuresi" , 100 , 10 , "pinustro" , 100 , 10 ,
    "poputrem"   ,     100 ,                    10 , "querelli" , 100 , 10 , "querrubr" , 100 , 10 ,
    "thujocci"   ,     100 ,                    10 , "tiliamer" , 100 , 10
  )
  scrpple_species_file <- prepSpeciesData(
    df = scrpple_species,
    type = "fire",
    path = scen_dir,
    filename = "SCRPPLE_Spp_Table.csv"
  )
  scrpple_supp <- tibble::tribble(
    ~IgnitionType        , ~Mapcode     , ~Suppress_Category_0 , ~FWI_Break_1 ,
    ~Suppress_Category_1 , ~FWI_Break_2 , ~Suppress_Category_2 ,
    "Accidental"         ,            1 ,                   10 ,           20 , 20 , 30 , 0 ,
    "Lightning"          ,            1 ,                   10 ,           20 , 20 , 30 , 0 ,
    "Rx"                 ,            1 ,                   10 ,           20 , 20 , 30 , 0
  )
  scrpple_supp_file <- prepSuppression_CSV_File(
    scrpple_supp,
    path = scen_dir,
    filename = "Suppression_Input.csv"
  )
  rast <- function(name) file.path(scen_dir, name)
  ext_scf <- SocialClimateFire$new(
    path = scen_dir,
    Timestep = 1L,
    Species_CSV_File = scrpple_species_file,
    AccidentalIgnitionsMap = rast("ecoregions.tif"),
    LightningIgnitionsMap = rast("ecoregions.tif"),
    RxIgnitionsMap = rast("ecoregions.tif"),
    AccidentalSuppressionMap = rast("ecoregions.tif"),
    LightningSuppressionMap = rast("ecoregions.tif"),
    RxSuppressionMap = rast("ecoregions.tif"),
    GroundSlopeMap = rast("ecoregions.tif"),
    UphillSlopeAzimuthMap = rast("ecoregions.tif"),
    ClayMap = rast("ecoregions.tif"),
    LightningIgnitionsCoeffs = c(-8.5, 0.03),
    AccidentalIgnitionsCoeffs = c(-8.5, 0.03),
    IgnitionDistribution = "ZeroInflatedPoisson",
    LightningIgnitionsBinomialCoeffs = c(-8.5, 0.03),
    AccidentalIgnitionsBinomialCoeffs = c(-8.5, 0.03),
    MaximumFineFuels = 500.0,
    MaximumRxWindSpeed = 80.0,
    MaximumRxFireWeatherIndex = 80.0,
    MinimumRxFireWeatherIndex = 1.0,
    MaximumRxTemperature = 35.0,
    MinimumRxRelativeHumidity = 22.0,
    MaximumRxFireIntensity = 1,
    NumberRxAnnualFires = 10,
    NumberRxDailyFires = 1,
    FirstDayRxFires = 2,
    LastDayRxFires = 300,
    TargetRxSize = 40,
    MaximumSpreadAreaCoeffs = c(10, -2.5, -2.5),
    SpreadProbabilityCoeffs = c(-1.79, 0.06, -0.915, 0.0126),
    SiteMortalityCoeffs = c(0.0059, 0.00050, -0.000010, -0.0002200, -0.00000050, 0.00000, 0.00000),
    CohortMortalityCoeffs = c(-0.703, -0.9908, 0.009),
    LadderFuelMaxAge = 40,
    LadderFuelSpeciesList = c("abiebals", "pinubank"),
    SuppressionMaxWindSpeed = 100,
    Suppression_CSV_File = scrpple_supp_file,
    DeadWoodTable = data.frame(species = "pinubank", age = 22)
  )

  ## ---- Biomass output extensions -----------------------------------------
  ## OutputBiomassPnET is intentionally omitted -- it requires PnET-Succession.
  ext_ob <- OutputBiomass$new(path = scen_dir, Timestep = 10, Species = "all")
  ext_obc <- OutputBiomassCommunity$new(path = scen_dir, Timestep = 10L)
  ext_obba <- OutputBiomassByAge$new(
    path = scen_dir,
    Timestep = 10,
    Species = c("pinubank ageclass1(10-40)", "poputrem ageclass1(<50)")
  )
  reclass_maps <- list(
    forest = list(
      Conifers = c("abiebals", "piceglau", "pinubank", "pinustro", "thujocci"),
      Hardwoods = c("acerrubr", "acersacc", "betualle", "fraxamer", "poputrem"),
      Other = c("querelli", "querrubr", "tiliamer")
    )
  )
  ext_obr <- OutputBiomassReclass$new(
    path = scen_dir,
    Timestep = 10L,
    ReclassMaps = reclass_maps,
    MapFileNames = "outputs/biomass-reclass/{reclass-map-name}-{timestep}.tif"
  )

  ## ---- Filter, write, and assemble scenario.txt --------------------------
  exts <- list(ext_bs, ext_scf, ext_ob, ext_obc, ext_obba, ext_obr)
  exts <- filter_extensions_for_image(exts, allowed_classes)

  for (e in exts) {
    e$write()
  }

  climate_cfg <- LandisClimateConfig$new(path = scen_dir)
  climate_cfg$add_file("biomass-succession_ClimateGenerator.txt")

  scen <- scenario(
    name = scen_name,
    extensions = exts,
    climate_config = climate_cfg,
    path = scen_dir,
    CellLength = 100,
    DisturbancesRandomOrder = FALSE,
    Duration = 10,
    EcoregionsFiles = c(
      file.path(scen_dir, "ecoregions.txt"),
      file.path(scen_dir, "ecoregions.tif")
    ),
    RandomNumberSeed = 147,
    SpeciesInputFile = file.path(scen_dir, "Core_species_data.txt")
  )

  scen_dir
}

## ---------------------------------------------------------------------------
## Drive the per-image build for the AllExtension scenarios.
## ---------------------------------------------------------------------------

for (image_id in names(IMAGES)) {
  image_info <- IMAGES[[image_id]]
  for (sb in list(
    list(name = "necn_all_extension", builder = build_necn_all_extension),
    list(name = "pnet_all_extension", builder = build_pnet_all_extension)
  )) {
    p <- build_one(sb$name, image_id, image_info, sb$builder, out_dir)
    if (!is.null(p)) scenarios <- c(scenarios, p)
  }
}

## ---------------------------------------------------------------------------
## Emit the scenario directories so the workflow can iterate over them.
## ---------------------------------------------------------------------------
cat(scenarios, sep = "\n")
cat("\n")
