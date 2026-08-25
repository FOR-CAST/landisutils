## Pre-flight validation of a generated LANDIS-II scenario directory.
##
## LANDIS-II fails on bad inputs in the worst possible way: the run dies a few seconds into extension
## initialisation, the R-side runner reports only a non-zero exit with EMPTY stderr, and the real
## message is buried in `Landis-log.txt` inside a scratch directory. Under a calibration warm pool
## that failure is multiplied by the pool size and can burn hours before anyone reads a log. Worse,
## one whole class of defect (a vertically mirrored map) produces no error at all -- the run completes
## and the answer is wrong.
##
## This validates the scenario DIRECTORY rather than the R6 extension objects, because the two
## scenario-assembly entry points do not share a code path: [scenario()] builds from a list of
## [LandisExtension] objects, while [write_landis_scenario_file()] takes already-written paths and is
## what the Dynamic Fire calibration uses (it bulk-copies a template directory and swaps files, so no
## extension object ever describes the copied-in initial-communities map -- which is exactly the file
## the mirroring defect landed in).

#' Read a directive's value from a LANDIS-II configuration file
#'
#' LANDIS-II config files are `Directive  value` lines with `>>` starting a
#' comment that runs to end of line, and values quoted only when they contain
#' whitespace. This returns the first value found for `directive`, unquoted.
#'
#' @param file Character path to the configuration file.
#' @param directive Character. The directive name to look up.
#' @param default Value to return when the file is missing or the directive is
#'   absent. Defaults to `NA_character_`.
#'
#' @return A length-1 character value, or `default`.
#'
#' @keywords internal
landis_directive <- function(file, directive, default = NA_character_) {
  if (!fs::file_exists(file)) {
    return(default)
  }
  hit <- grep(
    paste0("^[[:space:]]*", directive, "[[:space:]]"),
    readLines(file, warn = FALSE),
    value = TRUE
  )
  if (length(hit) == 0L) {
    return(default)
  }
  value <- trimws(sub(
    ">>.*$",
    "",
    sub(paste0("^[[:space:]]*", directive, "[[:space:]]+"), "", hit[[1L]])
  ))
  .unquote(value)
}

#' @keywords internal
.unquote <- function(x) {
  sub('^"(.*)"$', "\\1", trimws(x))
}

## Extensions treated as maps. LANDIS-II reads rasters through GDAL and accepts whatever GDAL
## opens, so this is deliberately not just GeoTIFF: the upstream Core8 reference scenarios use
## ERDAS IMAGINE (`.img`) for ecoregion and ignition maps, and `.gis` appears in others.
##
## Getting this list wrong is not a cosmetic problem. When it held only "tif", every scenario whose
## ecoregion map was an `.img` had no map to find, `EcoregionsMap` resolved to nothing, and the
## resulting complaint failed the whole scenario -- which silently cut the integration harness from
## 9 scenarios to 2 while it went on reporting success. Prefer adding an extension here over
## rejecting a scenario this package simply did not recognise.
.LANDIS_RASTER_EXTENSIONS <- c(
  "asc",
  "bil",
  "bin",
  "bip",
  "bsq",
  "gis",
  "grd",
  "img",
  "tif",
  "tiff",
  "vrt"
)

#' @keywords internal
.landis_is_raster <- function(paths) {
  tolower(tools::file_ext(paths)) %in% .LANDIS_RASTER_EXTENSIONS
}

## Map directives whose raster is a CODE map: 0 means inactive, non-zero means an active cell
## carrying a category (ecoregion, community, fire region). Only these can be compared against the
## ecoregion active mask -- see .landis_check_orientation().
.LANDIS_CODE_MAP_DIRECTIVES <- c(
  "EcoregionsMap",
  "InitialCommunitiesMap",
  "InitialFireEcoregionsMap",
  "InitialFireRegionsMap"
)

## Map directives whose raster is CONTINUOUS: 0 is a legitimate value on an active cell, so the
## non-zero mask is not a footprint and comparing it against the ecoregion mask says nothing.
## Measured on NRD_Quesnel, uphill slope azimuth agrees with the ecoregion mask on 0.5929 of cells
## as-is and 0.5929 flipped -- no discrimination whatsoever. These are checked for existence, pixel
## type and dimensions, but deliberately not for orientation.
.LANDIS_CONTINUOUS_MAP_DIRECTIVES <- c(
  "AccidentalIgnitionsMap",
  "AccidentalSuppressionMap",
  "ClayMap",
  "GroundSlopeFile",
  "GroundSlopeMap",
  "InputMap",
  "LightningIgnitionsMap",
  "LightningSuppressionMap",
  "LitterMap",
  "RxIgnitionsMap",
  "RxSuppressionMap",
  "RxZonesMap",
  "UphillSlopeAzimuthMap",
  "WoodyDebrisMap",
  "ZoneMap"
)

## Directives naming files LANDIS-II WRITES. These must not be checked for existence: at validation
## time the run has not happened yet. `output_manifest.txt` covers the fixed-name outputs; this
## covers the templated ones and anything not in the manifest.
.LANDIS_OUTPUT_DIRECTIVES <- c(
  "BiomassMaps",
  "BDPMapNames",
  "CalibrateMode",
  "EventLog",
  "LogFile",
  "MapFileNames",
  "MapNames",
  "NRDMapNames",
  "OutputMapName",
  "OutputsOfRoadLog",
  "OutputsOfRoadNetworkMaps",
  "PctConiferFileName",
  "PctDeadFirFileName",
  "PrescriptionMaps",
  "SiteLog",
  "SRDMapNames",
  "SummaryLog",
  "SummaryLogFile"
)

#' Validate a generated LANDIS-II scenario directory before running it
#'
#' Checks a fully assembled scenario directory for the input defects that
#' LANDIS-II either reports unhelpfully or does not report at all. It is a pure-R
#' guard: no Docker, no simulation. Run it once per scenario, not once per
#' replicate -- replicates copy an already-validated directory.
#'
#' Checks performed:
#'
#' \itemize{
#'   \item **Existence and non-emptiness** of every input file referenced by
#'     `scenario.txt` and by each extension configuration it names. Files
#'     LANDIS-II will *write* are excluded, via `output_manifest.txt` and an
#'     internal list of output-naming directives.
#'   \item **Pixel type** of every map: LANDIS-II opens rasters through
#'     `Landis.RasterIO.Gdal.GdalInputRaster.NewInputBand`, which accepts only
#'     GDAL `Byte`, `Int16`, `Int32`, `Float32` and `Float64`. See
#'     [landis_datatype()].
#'   \item **Dimensions**: every map must match the ecoregions map.
#'   \item **Orientation**: a map stored in the wrong row order relative to the
#'     ecoregions map is detected by comparing per-cell mask agreement against
#'     the agreement its vertically flipped self would achieve. See below.
#'   \item **Initial-communities integrity**: every map code present in the
#'     raster resolves to rows in the initial-communities CSV, allowing the one
#'     deliberately row-less empty-community code that
#'     [dedup_community_snapshot()] creates; and the CSV is small enough that the
#'     LANDIS-II parser will not exhaust the container's memory reading it.
#' }
#'
#' @section Detecting a mirrored map:
#'
#' A map written in the wrong row order is the most dangerous defect in this set,
#' because nothing rejects it: dimensions, values and totals are all correct, and
#' the run completes with the vegetation displaced relative to the ecoregion,
#' fire-region and topography maps. It cost a 25-generation Dynamic Fire
#' calibration.
#'
#' Orientation metadata cannot catch it. The mirrored map is written back by
#' `terra` and is north-up; only its *content* is reversed. So the check compares
#' content: for each code map, the fraction of cells whose active/inactive state
#' matches the ecoregions map, against the same fraction for the map's flipped
#' self. A correctly oriented map scores higher as-is; a mirrored one scores
#' higher flipped, and flipping swaps the pair exactly. No absolute threshold is
#' involved, which is what makes this robust: it is two measurements of the same
#' landscape rather than a tuned constant.
#'
#' Measured on the two assembled BC_HRV scenarios, as-is versus flipped:
#' initial communities 0.9720/0.7605 and 0.9798/0.4945; fire ecoregions
#' 1.0000/0.7799 and 1.0000/0.4940.
#'
#' Note this is deliberately NOT the stricter "every active cell carries a map
#' code". Measured on those same working scenarios, 10,897 and 95,063 active
#' ecoregion cells carry no initial-communities code -- they are cells with no
#' cohorts, which Biomass Succession handles -- so the strict form would reject
#' valid production input.
#'
#' Maps read through [read_landis_raster()], never `terra::rast()`, so the
#' comparison is made in the row order LANDIS-II itself will read.
#'
#' @param path Character. Path to the assembled scenario directory.
#' @param scenario_file Character. Name of the master scenario file within
#'   `path`. Defaults to `"scenario.txt"`.
#' @param error Logical. When `TRUE` (default), stop with every problem found.
#'   When `FALSE`, return them instead -- use this to survey scenarios without
#'   failing, e.g. when introducing a new check.
#' @param max_ic_csv_mb Numeric. Size above which an initial-communities CSV is
#'   reported. The LANDIS-II parser builds one `ExpandoObject` per row and costs
#'   a large multiple of the file size, so a per-pixel (undeduplicated) snapshot
#'   aborts with `System.OutOfMemoryException` before the simulation starts.
#'
#' @return Invisibly, a character vector of problems -- empty when the scenario
#'   is clean. With `error = TRUE` a non-empty result is raised instead.
#'
#' @family LANDIS-II execution helpers
#' @seealso [scenario()], [write_landis_scenario_file()], [landis_datatype()],
#'   [read_landis_raster()], [dedup_community_snapshot()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' validate_landis_scenario("LANDIS-II/hrv_biomass_fire/Chine")
#'
#' ## survey without failing, when introducing a new check
#' validate_landis_scenario(scenario_dir, error = FALSE)
#' }
validate_landis_scenario <- function(
  path,
  scenario_file = "scenario.txt",
  error = TRUE,
  max_ic_csv_mb = 200
) {
  stopifnot(is.character(path), length(path) == 1L)
  problems <- character(0)
  add <- function(...) problems <<- c(problems, paste0(...))

  if (!fs::dir_exists(path)) {
    return(.landis_validation_result(paste0("scenario directory not found: ", path), error, path))
  }
  scen <- fs::path(path, scenario_file)
  if (!fs::file_exists(scen)) {
    return(.landis_validation_result(paste0("no ", scenario_file, " in ", path), error, path))
  }

  ## Config files named by scenario.txt: the extension init files, plus the species and ecoregions
  ## files the core itself reads.
  configs <- .landis_scenario_configs(path, scenario_file)
  refs <- .landis_referenced_inputs(path, c(scenario_file, configs))

  ## --- existence and non-emptiness ---------------------------------------------------------------
  for (i in seq_len(nrow(refs))) {
    f <- refs$abs[[i]]
    where <- paste0(refs$config[[i]], " [", refs$directive[[i]], "]")
    if (!fs::file_exists(f)) {
      ## Backstop for output directives not named in .LANDIS_OUTPUT_DIRECTIVES. In a freshly built
      ## scenario every INPUT has been staged, so its parent directory exists; a referenced path
      ## whose parent directory does not exist yet is somewhere LANDIS-II will create at run time
      ## (`output/disturbances/roads/roadNetwork.tif` and the like).
      ##
      ## Enumerating directive names alone proved too brittle: a single unlisted one
      ## (OutputsOfRoadNetworkMaps) failed an entire scenario and took it out of the integration
      ## harness. Missing a real absent input is the lesser error -- LANDIS-II reports that one
      ## clearly, which is more than it does for anything else this function checks.
      if (!fs::dir_exists(fs::path_dir(f))) {
        next
      }
      add(where, ": file not found: ", refs$value[[i]])
    } else if (fs::file_size(f) == 0) {
      add(where, ": file is empty: ", refs$value[[i]])
    }
  }

  maps <- refs[.landis_is_raster(refs$abs) & fs::file_exists(refs$abs), ]

  ## --- pixel type --------------------------------------------------------------------------------
  for (i in seq_len(nrow(maps))) {
    ## suppressWarnings: an unopenable file makes terra warn before it errors, and the error is
    ## what we report -- the warning just doubles it up in the log.
    dt <- suppressWarnings(tryCatch(
      terra::datatype(terra::rast(maps$abs[[i]])),
      error = function(e) NA_character_
    ))
    if (is.na(dt[[1L]])) {
      add(maps$config[[i]], " [", maps$directive[[i]], "]: cannot open map: ", maps$value[[i]])
    } else if (!dt[[1L]] %in% .LANDIS_DATATYPES) {
      add(
        maps$config[[i]],
        " [",
        maps$directive[[i]],
        "]: pixel type ",
        dt[[1L]],
        " is not one LANDIS-II can read (",
        paste(.LANDIS_DATATYPES, collapse = ", "),
        "); see landis_datatype(): ",
        maps$value[[i]]
      )
    }
  }

  ## Read each map's active mask once: several checks need them and the largest BC_HRV landscape is
  ## 4.7M cells per map.
  masks <- stats::setNames(lapply(maps$abs, .landis_cell_mask), maps$abs)

  ## --- dimensions and orientation, against the ecoregions map ------------------------------------
  eco <- maps[maps$directive == "EcoregionsMap", ]
  if (nrow(eco) == 0L) {
    ## NOT a problem: this is a gap in what this function can see, not evidence that the scenario is
    ## wrong, and treating it as a failure is what broke the integration harness. Say so and skip
    ## the checks that need a reference map; the existence and pixel-type checks above still ran.
    message(
      "validate_landis_scenario(): no readable ecoregions map in ",
      path,
      "; geometry and orientation checks skipped."
    )
  } else {
    problems <- c(problems, .landis_check_geometry(maps, eco$abs[[1L]], masks))
  }

  ## --- initial communities -----------------------------------------------------------------------
  ## The ecoregion mask scopes the map-code check to cells LANDIS-II will actually initialise.
  .eco_mask <- if (nrow(eco) > 0L) masks[[eco$abs[[1L]]]] else NULL
  problems <- c(
    problems,
    .landis_check_initial_communities(path, refs, max_ic_csv_mb, .eco_mask)
  )

  ## --- per-extension contracts -------------------------------------------------------------------
  problems <- c(problems, .landis_check_extensions(path, configs, refs, masks))

  .landis_validation_result(problems, error, path)
}

#' @keywords internal
.landis_validation_result <- function(problems, error, path) {
  if (length(problems) > 0L && isTRUE(error)) {
    stop(
      "LANDIS-II scenario failed pre-flight validation: ",
      path,
      "\n",
      paste0("  - ", problems, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(problems)
}

## The extension configuration files (and core input files) named by scenario.txt. The extension
## tables are `   "Extension Name"        file.txt` rows, so pick the token ending in .txt after the
## quoted name rather than parsing the table structure.
#' @keywords internal
.landis_scenario_configs <- function(path, scenario_file) {
  lines <- .landis_config_lines(fs::path(path, scenario_file))
  ext <- regmatches(lines, regexpr('"[^"]+"[[:space:]]+[^[:space:]]+\\.txt', lines))
  ext <- .unquote(sub('^"[^"]+"[[:space:]]+', "", ext))
  unique(ext[nzchar(ext)])
}

## Strip `>>` comments and blank lines from a config file.
#' @keywords internal
.landis_config_lines <- function(file) {
  if (!fs::file_exists(file)) {
    return(character(0))
  }
  lines <- sub(">>.*$", "", readLines(file, warn = FALSE))
  lines[nzchar(trimws(lines))]
}

## Every INPUT file referenced from the given config files, as a data.frame of
## config / directive / value / abs. Grammar-agnostic on purpose: it scans for path-shaped tokens
## rather than knowing each of the 31 extensions' parameter names, so it covers extensions this
## package has no opinion about, and hand-assembled scenario directories.
#' @keywords internal
.landis_referenced_inputs <- function(path, configs) {
  manifest <- fs::path(path, "output_manifest.txt")
  produced <- if (fs::file_exists(manifest)) {
    fs::path_tidy(readLines(manifest, warn = FALSE))
  } else {
    character(0)
  }

  out <- list()
  for (cfg in unique(configs)) {
    for (line in .landis_config_lines(fs::path(path, cfg))) {
      tokens <- strsplit(trimws(line), "[[:space:]]+")[[1L]]
      if (length(tokens) < 2L) {
        next
      }
      directive <- .unquote(tokens[[1L]])
      if (directive %in% .LANDIS_OUTPUT_DIRECTIVES) {
        next
      }
      for (tok in .unquote(tokens[-1L])) {
        ## `{timestep}`-style templates name files LANDIS-II writes, not inputs
        if (!grepl("\\.(tif|csv|txt|img)$", tok, ignore.case = TRUE) || grepl("[{}]", tok)) {
          next
        }
        if (fs::path_tidy(tok) %in% produced) {
          next
        }
        out[[length(out) + 1L]] <- data.frame(
          config = cfg,
          directive = directive,
          value = tok,
          abs = as.character(fs::path(path, tok)),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(out) == 0L) {
    return(data.frame(
      config = character(0),
      directive = character(0),
      value = character(0),
      abs = character(0),
      stringsAsFactors = FALSE
    ))
  }
  refs <- do.call(rbind, out)
  refs[!duplicated(refs$abs), , drop = FALSE]
}

## Dimension + orientation checks for every map against the ecoregions map.
#' @keywords internal
.landis_check_geometry <- function(maps, eco_path, masks) {
  problems <- character(0)
  ref <- masks[[eco_path]]
  if (is.null(ref)) {
    return(paste0("scenario.txt [EcoregionsMap]: cannot read ecoregions map: ", eco_path))
  }

  for (i in seq_len(nrow(maps))) {
    if (identical(as.character(maps$abs[[i]]), as.character(eco_path))) {
      next
    }
    m <- masks[[maps$abs[[i]]]]
    where <- paste0(maps$config[[i]], " [", maps$directive[[i]], "]")
    if (is.null(m)) {
      problems <- c(problems, paste0(where, ": cannot read map: ", maps$value[[i]]))
      next
    }
    if (m$nrow != ref$nrow || m$ncol != ref$ncol) {
      problems <- c(
        problems,
        sprintf(
          "%s: map is %dx%d but the ecoregions map is %dx%d: %s",
          where,
          m$nrow,
          m$ncol,
          ref$nrow,
          ref$ncol,
          maps$value[[i]]
        )
      )
      next
    }
    if (!maps$directive[[i]] %in% .LANDIS_CODE_MAP_DIRECTIVES) {
      next ## continuous or unclassified: the non-zero mask is not a footprint
    }
    asis <- mean(m$mask == ref$mask)
    flipped <- mean(.landis_flip_mask(m$mask, m$nrow, m$ncol) == ref$mask)
    if (flipped > asis) {
      problems <- c(
        problems,
        sprintf(
          paste0(
            "%s: map appears VERTICALLY MIRRORED relative to the ecoregions map ",
            "(active-cell agreement %.4f as stored, %.4f flipped): %s. ",
            "A map derived from LANDIS-II output must be read with read_landis_raster(), ",
            "not terra::rast()."
          ),
          where,
          asis,
          flipped,
          maps$value[[i]]
        )
      )
    }
  }
  problems
}

## The active/inactive mask in the row order LANDIS-II will read the file, plus its dimensions.
## read_landis_raster() returns STORED row order for both north-up and geotransform-less files, which
## is exactly what LANDIS-II sees.
#' @keywords internal
.landis_cell_mask <- function(file) {
  r <- tryCatch(read_landis_raster(file), error = function(e) NULL)
  if (is.null(r)) {
    return(NULL)
  }
  v <- terra::values(r, mat = FALSE)
  v[is.na(v)] <- 0
  list(mask = v != 0, nrow = terra::nrow(r), ncol = terra::ncol(r))
}

#' @keywords internal
.landis_flip_mask <- function(mask, nr, nc) {
  m <- matrix(mask, nrow = nr, ncol = nc, byrow = TRUE)
  as.vector(t(m[nr:1L, , drop = FALSE]))
}

## Initial-communities map/CSV integrity.
#' @keywords internal
.landis_check_initial_communities <- function(path, refs, max_ic_csv_mb, eco_mask = NULL) {
  problems <- character(0)
  tif <- refs[refs$directive == "InitialCommunitiesMap", ]
  csv <- refs[refs$directive %in% c("InitialCommunities", "InitialCommunitiesCSV"), ]
  if (nrow(tif) == 0L || nrow(csv) == 0L) {
    return(problems)
  }
  tif <- tif$abs[[1L]]
  csv <- csv$abs[[1L]]
  if (!fs::file_exists(tif) || !fs::file_exists(csv)) {
    return(problems) ## already reported as missing
  }

  size_mb <- as.numeric(fs::file_size(csv)) / 1048576
  if (size_mb > max_ic_csv_mb) {
    problems <- c(
      problems,
      sprintf(
        paste0(
          "initial-communities CSV is %.0f MB (limit %.0f MB): %s. The LANDIS-II parser builds one ",
          "ExpandoObject per row and costs a large multiple of the file size, so a per-pixel ",
          "snapshot aborts with System.OutOfMemoryException before the simulation starts. ",
          "Collapse duplicate communities with dedup_community_snapshot()."
        ),
        size_mb,
        max_ic_csv_mb,
        basename(csv)
      )
    )
  }

  d <- tryCatch(data.table::fread(csv, fill = TRUE, showProgress = FALSE), error = function(e) NULL)
  if (is.null(d) || !("MapCode" %in% names(d))) {
    return(c(problems, paste0("initial-communities CSV has no MapCode column: ", basename(csv))))
  }
  r <- tryCatch(read_landis_raster(tif), error = function(e) NULL)
  if (is.null(r)) {
    return(problems) ## already reported as unreadable
  }
  ## A map code is only REACHABLE where the ecoregion map says the cell is active: LANDIS-II never
  ## resolves a community for an inactive cell. Scoping the test to active cells is what lets a
  ## landscape carry several deliberate non-vegetated land-cover codes without their being read as
  ## missing communities --  writes herb / shrub / bryoid / exposed /
  ## water codes into the initial-communities map, and none of them has CSV rows because none of
  ## them is a community. Measured on an 890,400-cell BC landscape, 140,597 cells carry four such
  ## codes and not one is ecoregion-active; both scenarios staged from that map run to completion.
  ##
  ## Without the mask (no readable ecoregions map) this falls back to testing the whole raster,
  ## which is the conservative direction: it can over-report, never under-report.
  v <- terra::values(r, mat = FALSE)
  v[is.na(v)] <- 0
  if (!is.null(eco_mask) && length(eco_mask$mask) == length(v)) {
    v <- v[eco_mask$mask]
  }
  present <- unique(v[v > 0])
  unresolved <- setdiff(present, unique(d[["MapCode"]]))
  ## One unresolved code is legitimate: dedup_community_snapshot() assigns a single shared code to
  ## active cells that have no cohorts, and that code deliberately has no CSV rows. More than one
  ## means codes are genuinely missing, and LANDIS-II aborts on load with "Unknown map code".
  if (length(unresolved) > 1L) {
    problems <- c(
      problems,
      sprintf(
        paste0(
          "%d initial-communities map code(s) have no rows in %s (e.g. %s). LANDIS-II aborts on ",
          "load with \"Unknown map code\". At most one such code is allowed (the shared ",
          "empty-community code)."
        ),
        length(unresolved),
        basename(csv),
        paste(utils::head(sort(unresolved), 5L), collapse = ", ")
      )
    )
  }
  problems
}

## --- per-extension contracts --------------------------------------------------------------------
##
## These read the WRITTEN configuration rather than the R6 objects that produced it, so they also
## cover the Dynamic Fire calibration: its DEoptim workers copy a template directory and PATCH
## dynamic-fire.txt with candidate parameters, so by the time a trial runs, no extension object
## describes the file that will actually be parsed.

#' @keywords internal
.landis_check_extensions <- function(path, configs, refs, masks) {
  problems <- character(0)
  for (cfg in configs) {
    ext <- landis_directive(fs::path(path, cfg), "LandisData")
    if (is.na(ext)) {
      next
    }
    if (ext == "Dynamic Fire System") {
      problems <- c(problems, .landis_check_dynamic_fire(path, cfg, refs, masks))
    } else if (ext == "Dynamic Fuel System") {
      problems <- c(problems, .landis_check_dynamic_fuels(path, cfg))
    }
  }
  problems
}

#' @keywords internal
.landis_check_dynamic_fire <- function(path, cfg, refs, masks) {
  problems <- character(0)
  file <- fs::path(path, cfg)

  ## Season proportions. The Dynamic Fire parser reads ProportionFire as SINGLE-precision floats and
  ## rejects the table with "Season Probabilities don't add to 1.0" unless they sum to exactly 1.
  ## Arbitrary decimal proportions (e.g. observed counts / total) only sum to 1 in double arithmetic
  ## and fail the float check unpredictably, so insertSeasonTable() quantises to dyadic fractions --
  ## exactly representable in float, and order-independent when summed. This checks what actually
  ## landed in the file, which a calibration patch can change after the writer has run.
  season <- .landis_config_block(file, "SeasonTable")
  if (length(season) > 0L) {
    pf <- suppressWarnings(as.numeric(vapply(season, function(x) x[[3L]], character(1))))
    if (anyNA(pf)) {
      problems <- c(problems, paste0(cfg, " [SeasonTable]: ProportionFire is not numeric"))
    } else {
      scaled <- pf * 2^23 ## the float32 mantissa: a dyadic fraction at or below this is exact
      if (any(abs(scaled - round(scaled)) > 1e-6) || sum(pf) != 1) {
        problems <- c(
          problems,
          sprintf(
            paste0(
              "%s [SeasonTable]: ProportionFire values (%s) are not dyadic fractions summing to 1. ",
              "The parser checks the sum in single precision and aborts with \"Season Probabilities ",
              "don't add to 1.0\". Generate the table with insertSeasonTable(), which quantises to ",
              "k/128."
            ),
            cfg,
            paste(format(pf), collapse = ", ")
          )
        )
      }
    }
  }

  ## The fire-ecoregions map must cover every cell the core considers active. LANDIS-II reads raw
  ## cell values and rejects any fire-region code that is not in the fire-size table, so a
  ## core-active cell sitting outside the fire-region polygons aborts the run with "Unknown map
  ## code". align_fire_to_core() is what guarantees this; the check is that it was applied.
  fire_map <- refs[refs$directive == "InitialFireEcoregionsMap", ]
  eco_map <- refs[refs$directive == "EcoregionsMap", ]
  if (nrow(fire_map) > 0L && nrow(eco_map) > 0L) {
    fm <- masks[[fire_map$abs[[1L]]]]
    em <- masks[[eco_map$abs[[1L]]]]
    if (!is.null(fm) && !is.null(em) && length(fm$mask) == length(em$mask)) {
      gaps <- sum(em$mask & !fm$mask)
      if (gaps > 0L) {
        problems <- c(
          problems,
          sprintf(
            paste0(
              "%s [InitialFireEcoregionsMap]: %s cell(s) are active in the ecoregions map but have ",
              "no fire region. LANDIS-II aborts with \"Unknown map code\". Align the fire map to the ",
              "core active mask."
            ),
            cfg,
            format(gaps, big.mark = ",")
          )
        )
      }
    }
  }
  problems
}

#' @keywords internal
.landis_check_dynamic_fuels <- function(path, cfg) {
  ## Every modelled species needs a row in the FuelTypes table. The table is keyed by species, and a
  ## species that appears in none of its rows is simply absent from the fuels model -- silently, with
  ## nothing in the logs. The same defect put red alder in a fir fuel group in a sibling project.
  species_file <- landis_directive(fs::path(path, "scenario.txt"), "Species")
  if (is.na(species_file)) {
    return(character(0))
  }
  modelled <- .landis_species_codes(fs::path(path, species_file))
  rows <- .landis_config_block(fs::path(path, cfg), "FuelTypes")
  if (length(modelled) == 0L || length(rows) == 0L) {
    return(character(0))
  }
  covered <- unique(unlist(lapply(rows, function(tok) {
    ## `<index> <base type> <min> to <max> <species>...`
    at <- which(tok == "to")
    if (length(at) == 0L || length(tok) < at[[1L]] + 2L) {
      return(character(0))
    }
    tok[seq(at[[1L]] + 2L, length(tok))]
  })))
  missing <- setdiff(modelled, covered)
  if (length(missing) == 0L) {
    return(character(0))
  }
  sprintf(
    paste0(
      "%s [FuelTypes]: %d modelled species have no fuel type (%s). They are absent from the fuels ",
      "model, with nothing in the logs to say so."
    ),
    cfg,
    length(missing),
    paste(sort(missing), collapse = ", ")
  )
}

## Species codes from a core `LandisData "Species"` file: the first token of each data row.
#' @keywords internal
.landis_species_codes <- function(file) {
  lines <- .landis_config_lines(file)
  lines <- lines[!grepl("^[[:space:]]*LandisData", lines)]
  codes <- vapply(lines, function(l) strsplit(trimws(l), "[[:space:]]+")[[1L]][[1L]], character(1))
  unique(unname(codes))
}

## Rows of a named table block in a LANDIS-II config file, as token vectors.
##
## The blocks are `<Header>` followed by `>>` column captions, the rows, and a blank line. Comment
## and blank lines BEFORE the first row are captions; the first blank line AFTER a row ends the
## block. Returns a list of character vectors, empty when the header is absent.
#' @keywords internal
.landis_config_block <- function(file, header) {
  if (!fs::file_exists(file)) {
    return(list())
  }
  raw <- readLines(file, warn = FALSE)
  start <- which(trimws(raw) == header)
  if (length(start) == 0L || start[[1L]] >= length(raw)) {
    return(list())
  }
  rows <- list()
  for (j in seq(start[[1L]] + 1L, length(raw))) {
    line <- trimws(sub(">>.*$", "", raw[[j]]))
    if (!nzchar(line)) {
      if (length(rows) > 0L) {
        break
      }
      next
    }
    tok <- strsplit(line, "[[:space:]]+")[[1L]]
    rows[[length(rows) + 1L]] <- tok[nzchar(tok)]
  }
  rows
}
