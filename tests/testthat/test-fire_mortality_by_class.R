## Tests for the per-class mortality counts and the two exported helpers behind them:
## landis_dominant_cohorts() and fire_cohort_killed().

## The damage table every fixture writes: the oldest cohort killed, as a percent of longevity,
## against severity minus tolerance. Column order is insertFireDamageTable()'s.
.tbl <- data.frame(
  CohortAgePercentLongevity = c(10, 50, 85, 100),
  FireSeverityMinusFireTolerance = c(-2, -1, 0, 1)
)

## Cohorts per map code. Sx has a tolerance but no longevity, Pl a longevity but no tolerance, so
## neither can be dominant: code 3's dominant is Hw despite Sx's larger biomass, and code 5 has
## none. Code 4's two cohorts tie on biomass, and At comes first in the file.
.class_ic <- data.frame(
  MapCode = c(1L, 1L, 2L, 3L, 3L, 4L, 4L, 5L),
  SpeciesName = c("Hw", "At", "Hw", "Hw", "Sx", "At", "Hw", "Pl"),
  CohortAge = c(30L, 10L, 29L, 31L, 100L, 170L, 200L, 40L),
  CohortBiomass = c(100, 5, 100, 100, 500, 50, 50, 100)
)

## Severity map values per cell, one vector per timestep: 0 inactive, 1 unburned, 2 burned with no
## cohort damaged, and severity class + 2 for a damaged cell.
.class_sev <- list(c(3L, 3L, 3L, 3L, 3L, 1L, 2L, 0L), c(4L, 7L, 4L, 1L, 7L, 4L, 3L, 0L))

## A 1 x 8 replicate directory: one row of cells, so the reader's vertical flip cannot reorder them.
## The core species file is named by scenario.txt's `Species` directive, and the Dynamic Fire
## species table by dynamic-fire.txt's `Species_CSV_File` directive, as this package's writers do.
.mk_class_rep <- function(
  sev = .class_sev,
  ic = .class_ic,
  map = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 1L),
  env = parent.frame()
) {
  dir <- withr::local_tempdir(.local_envir = env)
  spp <- prepSpeciesData(
    data.frame(
      SpeciesCode = c("Hw", "At", "Pl"),
      Longevity = c(300L, 200L, 250L),
      SexualMaturity = c(20L, 10L, 15L),
      SeedDispDistEff = c(50L, 30L, 40L),
      SeedDispDistMax = c(1398L, 200L, 500L),
      VegReprodProb = c(0, 0.5, 0),
      SproutAgeMin = c(0L, 10L, 0L),
      SproutAgeMax = c(0L, 100L, 0L),
      PostFireRegen = c("none", "resprout", "serotiny")
    ),
    type = "core",
    path = dir
  )
  writeLines(
    c(insertLandisData("Scenario"), insertSpeciesDataFile(fs::path_file(spp), core = TRUE)),
    fs::path(dir, "scenario.txt")
  )
  writeLines(
    c(
      insertLandisData("Dynamic Fire System"),
      insertFile("Species_CSV_File", "dynamic-fire-species.csv"),
      insertFireDamageTable(.tbl)
    ),
    fs::path(dir, "dynamic-fire.txt")
  )
  utils::write.csv(
    data.frame(SpeciesCode = c("Hw", "At", "Sx"), FireTolerance = c(3L, 1L, 2L)),
    fs::path(dir, "dynamic-fire-species.csv"),
    row.names = FALSE
  )
  utils::write.csv(ic, fs::path(dir, "initial-communities.csv"), row.names = FALSE)
  r <- terra::rast(nrows = 1, ncols = 8, xmin = 0, xmax = 8, ymin = 0, ymax = 1)
  terra::values(r) <- map
  terra::writeRaster(r, fs::path(dir, "initial-communities.tif"), datatype = "INT2S")
  fs::dir_create(fs::path(dir, "fire"))
  for (i in seq_along(sev)) {
    sv <- terra::rast(r)
    terra::values(sv) <- sev[[i]]
    terra::writeRaster(sv, fs::path(dir, "fire", sprintf("severity-%d.tif", i)), datatype = "INT2S")
  }
  dir
}

## The dominant cohorts of the default fixture.
.class_dom <- data.frame(
  MapCode = 1:4,
  SpeciesName = c("Hw", "Hw", "Hw", "At"),
  CohortAge = c(30, 29, 31, 170),
  CohortBiomass = c(100, 100, 100, 50),
  longevity = c(300, 300, 300, 200),
  tolerance = c(3, 3, 3, 1)
)

## bb00cf64f's (0.0.175's) landis_overstory_mortality_share() and .damage_age_pct(), verbatim but
## for the function's name, so the defaults can be held identical to the previous release.
.mortality_share_0175 <- function(rep_dir) {
  stopifnot(fs::dir_exists(rep_dir))
  fire_dir <- fs::path(rep_dir, "fire")
  if (!fs::dir_exists(fire_dir)) {
    return(NULL)
  }
  sev_files <- fs::dir_ls(fire_dir, regexp = "severity-\\d+\\.tif$", type = "file")
  ic_csv <- fs::path(rep_dir, "initial-communities.csv")
  ic_tif <- fs::path(rep_dir, "initial-communities.tif")
  ## Resolve the Dynamic Fire species table the way `dynamic-fire.txt` NAMES it, for exactly the
  ## reason the core species file below is resolved that way -- and this is the SAME defect, one file
  ## over. `DynamicFire$new()` writes this table as `dynamic-fire-species.csv` and records the name
  ## in the extension's own `Species_CSV_File` directive, so the hardcoded `DynamicFire_Spp_Table.csv`
  ## (the name only landisutils' own `df_input()` default uses) is absent from every scenario this
  ## package writes. The all-files guard then returned NULL again, and `comp_mortality` stayed a
  ## STRUCTURAL ZERO through the 2026-10-09 search AND through the 2026-10-20 baseline validation
  ## that was run to measure it: `overstory_mortality` was NULL in all 10 replicates, with the core
  ## species file resolving correctly. Fixing one of the two names in 0.0.171 moved the failure
  ## rather than removing it, which is why both are now read from their directives.
  spp_csv <- .calibration_df_species_file(rep_dir)
  ## Resolve the species file the way the scenario NAMES it, not by guessing. `writeSpecies()`
  ## defaults to `species-core.txt`, so a hardcoded `species.txt` is absent from every scenario this
  ## package writes, the all-files guard below returns NULL, and the component scores a STRUCTURAL
  ## ZERO -- identical for every parameter set, so nothing that reaches the loss only through it has
  ## any gradient. Measured on a completed 25-generation search (2026-10-09): `comp_mortality` was
  ## exactly 0 in all 519 evaluations and `overstory_mortality` NULL in all 20 validation replicates,
  ## leaving `DamageAgeMultiplier` unconstrained at rho -0.036 to the total loss. The scenario builder
  ## in this same file already reads the `Species` directive (`.calibration_species_file()`); this is
  ## the same lookup, with the old hardcoded name kept only as the fallback.
  spp_txt <- .calibration_species_file(rep_dir)
  fire_txt <- fs::path(rep_dir, "dynamic-fire.txt")
  if (
    length(sev_files) == 0L || !all(fs::file_exists(c(ic_csv, ic_tif, spp_csv, spp_txt, fire_txt)))
  ) {
    return(NULL)
  }

  tol <- .read_fire_tolerance(spp_csv)
  longevity <- .read_species_longevity(spp_txt)
  damage <- .read_fire_damage_table(fire_txt)

  ## Per map code, the age of the dominant cohort as a share of its species' longevity, and that
  ## species' tolerance -- all the severity-independent part of the decision, computed once.
  ic <- utils::read.csv(ic_csv)
  stopifnot(all(c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass") %in% names(ic)))
  ic <- ic[ic$SpeciesName %in% names(tol) & ic$SpeciesName %in% names(longevity), , drop = FALSE]
  if (nrow(ic) == 0L) {
    return(NULL)
  }
  ic <- ic[order(ic$MapCode, -ic$CohortBiomass), , drop = FALSE]
  dom <- ic[!duplicated(ic$MapCode), , drop = FALSE]
  dom$age_share <- 100 * dom$CohortAge / longevity[dom$SpeciesName]
  dom$tol <- tol[dom$SpeciesName]

  ic_r <- read_landis_raster(as.character(ic_tif))
  codes <- terra::values(ic_r, mat = FALSE)

  burned_cells <- 0L
  high_cells <- 0L
  for (f in as.character(sev_files)) {
    sev <- terra::values(read_landis_raster(f), mat = FALSE)
    burned <- which(!is.na(sev) & sev >= 2)
    if (length(burned) == 0L) {
      next
    }
    burned_cells <- burned_cells + length(burned)
    ## Severity class from the map value, and the oldest cohort that class kills on this cell.
    class <- sev[burned] - 2L
    idx <- match(codes[burned], dom$MapCode)
    ok <- !is.na(idx)
    if (!any(ok)) {
      next
    }
    ## A class of 0 is the map's value 2: the extension burned the cell and damaged NO cohort
    ## (user guide 3.1). Its severity is not recorded, so re-deriving mortality from the damage
    ## table there would invent a severity of 0 and, at any tolerance the table reaches, score a
    ## share of those cells as having lost their dominant cohort -- against the extension's own
    ## statement that nothing was killed. They stay in the denominator, because the observed
    ## reference counts every assessed pixel inside a fire perimeter including the unburned ones.
    damaged <- class[ok] >= 1L
    diff <- class[ok] - dom$tol[idx[ok]]
    killed_to <- .damage_age_pct(damage, diff)
    high_cells <- high_cells + sum(damaged & (class[ok] >= 5 | dom$age_share[idx[ok]] <= killed_to))
  }
  list(
    burned_cells = burned_cells,
    high_cells = high_cells,
    share = if (burned_cells > 0L) high_cells / burned_cells else NA_real_
  )
}
## The oldest cohort killed (percent of longevity) at each severity-minus-tolerance difference
## (internal). Below the table's smallest difference nothing is killed; at or above its largest,
## everything is.
.damage_age_pct <- function(damage, diff) {
  out <- numeric(length(diff))
  hit <- findInterval(diff, damage$diff)
  out[hit == 0L] <- 0
  keep <- hit > 0L
  out[keep] <- damage$pct[hit[keep]]
  out
}

## landis_dominant_cohorts() -------------------------------------------------------------------

test_that("landis_dominant_cohorts() takes the largest cohort with tolerance and longevity", {
  d <- .mk_class_rep()
  expect_identical(landis_dominant_cohorts(d), .class_dom)
})

test_that("landis_dominant_cohorts() breaks a biomass tie by file order", {
  ## With code 4's Hw row ahead of its At row, the tie goes to Hw.
  d <- .mk_class_rep(ic = .class_ic[c(1:5, 7, 6, 8), ])
  dom <- landis_dominant_cohorts(d)

  expected <- .class_dom
  expected[4L, c("SpeciesName", "CohortAge", "longevity", "tolerance")] <- list("Hw", 200, 300, 3)
  expect_identical(dom, expected)
})

test_that("landis_dominant_cohorts() falls back to DynamicFire_Spp_Table.csv without a directive", {
  d <- .mk_class_rep()
  txt <- readLines(fs::path(d, "dynamic-fire.txt"))
  writeLines(txt[!grepl("Species_CSV_File", txt)], fs::path(d, "dynamic-fire.txt"))
  fs::file_move(fs::path(d, "dynamic-fire-species.csv"), fs::path(d, "DynamicFire_Spp_Table.csv"))

  expect_identical(landis_dominant_cohorts(d), .class_dom)
})

test_that("landis_dominant_cohorts() returns NULL without an input file or a usable cohort", {
  d <- .mk_class_rep()
  fs::file_delete(fs::path(d, "initial-communities.csv"))
  expect_null(landis_dominant_cohorts(d))

  d <- .mk_class_rep()
  fs::file_delete(fs::path(d, "species-core.txt"))
  expect_null(landis_dominant_cohorts(d))

  d <- .mk_class_rep()
  fs::file_delete(fs::path(d, "dynamic-fire-species.csv"))
  expect_null(landis_dominant_cohorts(d))

  ## Sx has no longevity and Pl no tolerance, so no row survives.
  d <- .mk_class_rep(ic = .class_ic[.class_ic$SpeciesName %in% c("Sx", "Pl"), ])
  expect_null(landis_dominant_cohorts(d))
})

test_that("landis_dominant_cohorts() stops on a missing dir or a missing IC column", {
  missing_dir <- fs::path(withr::local_tempdir(), "no-such-replicate")
  expect_error(landis_dominant_cohorts(missing_dir), "does not exist")

  d <- .mk_class_rep(ic = .class_ic[, c("MapCode", "SpeciesName", "CohortAge")])
  expect_error(landis_dominant_cohorts(d), "CohortBiomass")
})

## fire_cohort_killed() ------------------------------------------------------------------------

test_that("fire_cohort_killed() applies the row that severity minus tolerance picks", {
  ## 100 of 300 years is 33 % of longevity. Tolerance 3: class 1 is difference -2 (10 %, survives),
  ## class 2 is -1 (50 %, dies), and class 5 kills whatever the table says. The table is given in
  ## reverse order, which must not matter.
  expect_identical(
    fire_cohort_killed(c(1L, 2L, 5L), 100, 300, 3, .tbl[4:1, ]),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("fire_cohort_killed() follows the extension's float comparison under engine_rule", {
  ## 30 of 300 years is exactly the 10 % that class 1 at tolerance 3 kills to. In double precision
  ## 10 <= 10; the extension compares 0.1 with age / longevity in single precision, which rounds up
  ## to 0.100000001, so there the cohort survives.
  expect_true(fire_cohort_killed(1L, 30, 300, 3, .tbl))
  expect_false(fire_cohort_killed(1L, 30, 300, 3, .tbl, engine_rule = TRUE))
})

test_that("fire_cohort_killed() is FALSE below the table, at class 0 and on any missing input", {
  for (rule in c(FALSE, TRUE)) {
    ## Tolerance 4 at class 1 is difference -3, below every row: nothing dies, not even age 0.
    expect_identical(
      fire_cohort_killed(c(1L, 1L), c(0, 30), 300, 4, .tbl, engine_rule = rule),
      c(FALSE, FALSE)
    )
    ## Class 0 is a burned cell with no cohort damaged; nor is there a class 6 or a class -1.
    expect_identical(
      fire_cohort_killed(c(0L, 6L, -1L), 0, 300, 3, .tbl, engine_rule = rule),
      rep(FALSE, 3)
    )
    ## A missing input kills nothing, at class 5 too.
    expect_identical(
      fire_cohort_killed(
        c(NA, 5L, 5L, 5L, 2L),
        c(30, NA, 30, 30, NA),
        c(300, 300, NA, 300, 300),
        c(3, 3, 3, NA, 3),
        .tbl,
        engine_rule = rule
      ),
      rep(FALSE, 5)
    )
  }
  expect_identical(fire_cohort_killed(integer(0), 30, 300, 3, .tbl), logical(0))
})

test_that("fire_cohort_killed() stops on a bad table, engine_rule or input length", {
  expect_error(fire_cohort_killed(1L, 30, 300, 3, .tbl[0, ]), "no rows")
  expect_error(fire_cohort_killed(1L, 30, 300, 3, .tbl[, 1L, drop = FALSE]), "two columns")
  pct_text <- transform(.tbl, CohortAgePercentLongevity = paste0(CohortAgePercentLongevity, "%"))
  expect_error(fire_cohort_killed(1L, 30, 300, 3, pct_text), "numeric")
  expect_error(fire_cohort_killed(1L, 30, 300, 3, as.matrix(.tbl)), "must be a data.frame")
  na_tbl <- .tbl
  na_tbl[1L, 1L] <- NA
  expect_error(fire_cohort_killed(1L, 30, 300, 3, na_tbl), "no missing values")
  expect_error(fire_cohort_killed(1L, 30, 300, 3, .tbl, engine_rule = NA), "engine_rule")
  expect_error(fire_cohort_killed(1L, 30, 300, 3, .tbl, engine_rule = "yes"), "engine_rule")
  expect_error(fire_cohort_killed(1:3, c(30, 40), 300, 3, .tbl), "length")
})

## landis_overstory_mortality_share(by_class = , engine_rule = ) -------------------------------

test_that("landis_overstory_mortality_share() counts burned cells by timestep, code and class", {
  d <- .mk_class_rep()
  dom <- landis_dominant_cohorts(d)
  m <- landis_overstory_mortality_share(d, by_class = TRUE)

  expect_named(m, c("burned_cells", "high_cells", "share", "by_class"))
  expect_identical(m$burned_cells, 12L)
  expect_identical(
    m$by_class,
    tibble::tibble(
      timestep = rep(1:2, c(6L, 5L)),
      MapCode = c(1L, 2L, 2L, 3L, 4L, 5L, 1L, 2L, 2L, 3L, 5L),
      class = c(1L, 0L, 1L, 1L, 1L, 1L, 2L, 1L, 5L, 2L, 5L),
      cells = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L)
    )
  )
  expect_identical(sum(m$by_class$cells), m$burned_cells)
  ## The counts add to the share and leave its other fields as they were.
  expect_identical(m[1:3], landis_overstory_mortality_share(d))

  ## The kill rule applied to the counts gives the share's own numerator, under either rule.
  ## Under engine_rule, code 1 at class 1 (exactly 10 %) and code 4 at class 1 (exactly 85 %)
  ## survive, so 8 lost dominant cohorts become 6.
  expected <- list(`FALSE` = list(high = 8L, share = 8 / 12), `TRUE` = list(high = 6L, share = 0.5))
  for (rule in c(FALSE, TRUE)) {
    mr <- landis_overstory_mortality_share(d, by_class = TRUE, engine_rule = rule)
    b <- mr$by_class
    i <- match(b$MapCode, dom$MapCode)
    killed <- fire_cohort_killed(
      b$class,
      dom$CohortAge[i],
      dom$longevity[i],
      dom$tolerance[i],
      .tbl,
      engine_rule = rule
    )
    expect_identical(sum(b$cells[killed]), mr$high_cells)
    expect_identical(mr$high_cells, expected[[as.character(rule)]]$high)
    expect_identical(mr$share, expected[[as.character(rule)]]$share)
    expect_identical(b, m$by_class)
  }
})

test_that("by_class keeps burned cells with no map code", {
  d <- .mk_class_rep(
    sev = list(c(3L, 1L, 1L, 1L, 1L, 1L, 1L, 3L)),
    map = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, NA)
  )
  m <- landis_overstory_mortality_share(d, by_class = TRUE)

  expect_identical(
    m$by_class,
    tibble::tibble(timestep = 1L, MapCode = c(1L, NA), class = 1L, cells = 1L)
  )
  expect_identical(m$burned_cells, 2L)
  expect_identical(m$high_cells, 1L)
})

test_that("an unburned replicate has a zero share and an empty by_class", {
  d <- .mk_class_rep(sev = list(c(1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L)))
  m <- landis_overstory_mortality_share(d, by_class = TRUE)

  expect_identical(m[1:3], list(burned_cells = 0L, high_cells = 0L, share = NA_real_))
  expect_identical(
    m$by_class,
    tibble::tibble(timestep = integer(), MapCode = integer(), class = integer(), cells = integer())
  )
})

test_that("landis_overstory_mortality_share() with defaults is identical to 0.0.175's", {
  same <- function(d) {
    out <- landis_overstory_mortality_share(d)
    expect_identical(out, .mortality_share_0175(d))
    out
  }

  d <- .mk_class_rep()
  expect_identical(same(d), list(burned_cells = 12L, high_cells = 8L, share = 8 / 12))

  ## The two fixtures above: a cell with no map code, and nothing burned.
  same(.mk_class_rep(sev = list(c(3L, 1L, 1L, 1L, 1L, 1L, 1L, 3L)), map = c(1:5, 1L, 2L, NA)))
  same(.mk_class_rep(sev = list(c(1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L))))
  ## A biomass tie that goes the other way.
  same(.mk_class_rep(ic = .class_ic[c(1:5, 7, 6, 8), ]))

  ## These five are NULL.
  expect_null(same(.mk_class_rep(sev = list())))
  expect_null(same(.mk_class_rep(ic = .class_ic[.class_ic$SpeciesName %in% c("Sx", "Pl"), ])))
  d <- .mk_class_rep()
  fs::dir_delete(fs::path(d, "fire"))
  expect_null(same(d))
  d <- .mk_class_rep()
  fs::file_delete(fs::path(d, "dynamic-fire-species.csv"))
  expect_null(same(d))
  d <- .mk_class_rep()
  fs::file_delete(fs::path(d, "dynamic-fire.txt"))
  expect_null(same(d))

  ## The core species file's fallback name, with no `Species` directive to read.
  d <- .mk_class_rep()
  fs::file_move(fs::path(d, "species-core.txt"), fs::path(d, "species.txt"))
  fs::file_delete(fs::path(d, "scenario.txt"))
  expect_identical(same(d)$high_cells, 8L)

  ## The species table's fallback name, with no `Species_CSV_File` directive to read.
  d <- .mk_class_rep()
  txt <- readLines(fs::path(d, "dynamic-fire.txt"))
  writeLines(txt[!grepl("Species_CSV_File", txt)], fs::path(d, "dynamic-fire.txt"))
  fs::file_move(fs::path(d, "dynamic-fire-species.csv"), fs::path(d, "DynamicFire_Spp_Table.csv"))
  expect_identical(same(d)$high_cells, 8L)
})

test_that("landis_overstory_mortality_share() stops on a flag that is not TRUE or FALSE", {
  d <- .mk_class_rep()
  expect_error(landis_overstory_mortality_share(d, by_class = "yes"), "by_class")
  expect_error(landis_overstory_mortality_share(d, by_class = NA), "by_class")
  expect_error(landis_overstory_mortality_share(d, engine_rule = c(TRUE, FALSE)), "engine_rule")
})

## bb00cf64f's (0.0.175's) parse_dynamic_fire_logs(), verbatim but for calling .mortality_share_0175(),
## so the defaults can be held identical to the previous release.
.parse_logs_0175 <- function(rep_dir, pixel_area_ha = 1.0) {
  stopifnot(fs::dir_exists(rep_dir), is.numeric(pixel_area_ha), pixel_area_ha > 0)
  event_path <- fs::path(rep_dir, "fire", "dynamic-fire-event-log.csv")
  summary_path <- fs::path(rep_dir, "fire", "dynamic-fire-summary-log.csv")
  if (!fs::file_exists(event_path) || !fs::file_exists(summary_path)) {
    stop("Dynamic Fire logs not found under ", fs::path(rep_dir, "fire"), call. = FALSE)
  }

  events <- .read_landis_csv(event_path)
  summary_df <- .read_landis_csv(summary_path)

  n_fires_by_year <- summary_df |>
    dplyr::group_by(year = as.integer(.data$Time)) |>
    dplyr::summarise(n_fires = sum(.data$NumberFires, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$year) |>
    .drop_initial_timestep()

  if (nrow(events) > 0L) {
    if (!all(c("SitesChecked", "DamagedSites") %in% names(events))) {
      stop(
        "Dynamic Fire event log ",
        event_path,
        " lacks SitesChecked or DamagedSites; the burned area and mean severity cannot be recovered",
        call. = FALSE
      )
    }
    ## A fire's burned area is `SitesChecked`, not `DamagedSites`. The published extension logs
    ## `DamagedSites` as one more than the cells the fire burned, on every event: a fire that
    ## burns only its ignition cell logs 2, and the burned cells on that timestep's severity map
    ## equal `SitesChecked` exactly. `MeanSeverity` is the summed severity divided by that same
    ## inflated count, so it is rescaled onto the burned cells; left alone it reads 0.5 for a
    ## one-cell fire at severity 1 and 1.5 for one at severity 3. A build with the count fixed
    ## (Extension-Dynamic-Fire-System issue #15) logs `DamagedSites == SitesChecked`, where the
    ## same rescaling is a factor of 1.
    checked <- as.integer(events$SitesChecked)
    damaged <- as.integer(events$DamagedSites)
    ## The reading below rests on one of those two relations holding on every event of the log,
    ## each verified against the severity maps. Say so if a log breaks it, rather than silently
    ## scoring sizes on an assumption that no longer holds. A warning, not an error, so it
    ## cannot strand a long calibration.
    gap <- damaged - checked
    off <- if (gap[1L] %in% c(0L, 1L)) which(gap != gap[1L]) else seq_along(gap)
    if (length(off) > 0L) {
      warning(
        "Dynamic Fire event log ",
        event_path,
        ": DamagedSites is not SitesChecked + 1 (published extension) or SitesChecked (fixed ",
        "extension) consistently on ",
        length(off),
        " of ",
        length(damaged),
        " events (first at row ",
        off[1L],
        "); fire sizes are taken from SitesChecked, which matched the severity maps only where ",
        "one relation held throughout",
        call. = FALSE
      )
    }
    events_tbl <- tibble::tibble(
      year = as.integer(events$Time),
      eco = trimws(as.character(events$InitFireRegion)),
      init_fuel = as.integer(events$InitFuel),
      sites = checked,
      mean_severity = ifelse(
        checked > 0L,
        as.numeric(events$MeanSeverity) * damaged / checked,
        NA_real_
      )
    )
    fire_sizes_ha <- sort(as.numeric(events_tbl$sites) * pixel_area_ha)
  } else {
    events_tbl <- tibble::tibble(
      year = integer(0),
      eco = character(0),
      init_fuel = integer(0),
      sites = integer(0),
      mean_severity = numeric(0)
    )
    fire_sizes_ha <- numeric(0)
  }

  list(
    n_fires_by_year = n_fires_by_year,
    fire_sizes_ha = fire_sizes_ha,
    events = events_tbl,
    total_sites_burned = sum(events_tbl$sites),
    n_events = nrow(events_tbl),
    area_by_fuel_ha = .read_burned_area_by_fuel(rep_dir, pixel_area_ha),
    ## NULL from a mock simulator, or from a rep that kept no severity maps.
    overstory_mortality = .mortality_share_0175(rep_dir)
  )
}

## The sample event and summary logs, copied to `dir`/fire/ under the names the parser reads.
.add_sample_logs <- function(dir) {
  fs::dir_create(fs::path(dir, "fire"))
  for (f in c("event", "summary")) {
    fs::file_copy(
      system.file(
        "testdata",
        sprintf("dynamic-fire-%s-log-sample.csv", f),
        package = "landisutils"
      ),
      fs::path(dir, "fire", sprintf("dynamic-fire-%s-log.csv", f)),
      overwrite = TRUE
    )
  }
  dir
}

.parse_logs <- function(env = parent.frame()) {
  .add_sample_logs(withr::local_tempdir(.local_envir = env))
}

test_that("the default parse is identical to 0.0.175's, with and without a share", {
  logs <- .parse_logs()
  rep <- .add_sample_logs(.mk_class_rep())
  header_only <- .add_sample_logs(.mk_class_rep())
  ev_path <- fs::path(header_only, "fire", "dynamic-fire-event-log.csv")
  writeLines(readLines(ev_path)[1L], ev_path)
  for (x in list(logs, rep, header_only)) {
    for (a in c(1, 4)) {
      expect_identical(parse_dynamic_fire_logs(x, a), .parse_logs_0175(x, a))
    }
  }
  ## The rep really has a share, and the log-only directory none.
  expect_identical(parse_dynamic_fire_logs(rep)$overstory_mortality$high_cells, 8L)
  expect_null(parse_dynamic_fire_logs(logs)$overstory_mortality)
})

test_that("event_fields appends the five event fields after the default columns", {
  logs <- .parse_logs()
  def <- parse_dynamic_fire_logs(logs)
  ev <- parse_dynamic_fire_logs(logs, event_fields = TRUE)$events
  expect_equal(
    ev$selected_size,
    c(536.4556364142938, 2.820531471791978, 48.28743611183686, 2.820531471791978)
  )
  expect_identical(ev$season, c("0", "1", "1", "1"))
  expect_identical(ev$size_bin, c(3L, 1L, 2L, 1L))
  expect_identical(ev$init_row, c(534L, 619L, 195L, 619L))
  expect_identical(ev$init_col, c(375L, 276L, 151L, 276L))
  expect_type(ev$selected_size, "double")
  ## The five follow the default columns; the rest equals the default parse.
  fields <- c("selected_size", "season", "size_bin", "init_row", "init_col")
  expect_identical(names(ev), c(names(def$events), fields))
  expect_identical(ev[names(def$events)], def$events)
  expect_identical(
    parse_dynamic_fire_logs(logs, event_fields = TRUE)[setdiff(names(def), "events")],
    def[setdiff(names(def), "events")]
  )
})

test_that("event_fields gives typed, empty columns for a log with no events", {
  d <- .parse_logs()
  ev_path <- fs::path(d, "fire", "dynamic-fire-event-log.csv")
  writeLines(readLines(ev_path)[1L], ev_path)
  ev <- parse_dynamic_fire_logs(d, event_fields = TRUE)$events
  expect_identical(nrow(ev), 0L)
  expect_identical(ev$selected_size, numeric(0))
  expect_identical(ev$season, character(0))
  expect_identical(ev$size_bin, integer(0))
  expect_identical(ev$init_row, integer(0))
  expect_identical(ev$init_col, integer(0))
})

test_that("event_fields stops on a log that lacks a source column, naming it", {
  for (col in c(
    "SelectedSizeOrDuration",
    "FireSeason",
    "SizeBin",
    "InitSiteRow",
    "InitSiteColumn"
  )) {
    d <- .parse_logs()
    ev_path <- fs::path(d, "fire", "dynamic-fire-event-log.csv")
    lines <- readLines(ev_path)
    k <- match(col, strsplit(lines[1L], ",", fixed = TRUE)[[1L]])
    expect_false(is.na(k))
    writeLines(
      vapply(
        lines,
        ## The sentinel keeps a trailing empty field, which strsplit() would drop.
        function(l) {
          sub(
            "x$",
            "",
            paste(strsplit(paste0(l, "x"), ",", fixed = TRUE)[[1L]][-k], collapse = ",")
          )
        },
        character(1),
        USE.NAMES = FALSE
      ),
      ev_path
    )
    expect_error(parse_dynamic_fire_logs(d, event_fields = TRUE), paste("lacks", col), fixed = TRUE)
    ## The default parse does not need it.
    expect_no_error(parse_dynamic_fire_logs(d))
  }
})

test_that("by_class and engine_rule reach the share", {
  rep <- .add_sample_logs(.mk_class_rep())
  p <- parse_dynamic_fire_logs(rep, by_class = TRUE, engine_rule = TRUE, event_fields = TRUE)
  expect_identical(
    p$overstory_mortality,
    landis_overstory_mortality_share(rep, by_class = TRUE, engine_rule = TRUE)
  )
  expect_identical(p$overstory_mortality$high_cells, 6L)
  expect_true("by_class" %in% names(p$overstory_mortality))
  p1 <- parse_dynamic_fire_logs(rep, by_class = TRUE)
  expect_identical(p1$overstory_mortality, landis_overstory_mortality_share(rep, by_class = TRUE))
  expect_identical(p1$overstory_mortality$high_cells, 8L)
})

test_that("parse_dynamic_fire_logs() stops on a switch that is not TRUE or FALSE", {
  logs <- .parse_logs()
  expect_error(parse_dynamic_fire_logs(logs, event_fields = "yes"), "event_fields")
  expect_error(parse_dynamic_fire_logs(logs, by_class = NA), "by_class")
  expect_error(parse_dynamic_fire_logs(logs, engine_rule = c(TRUE, FALSE)), "engine_rule")
})

## A stand-in parser, in baseenv() so a child process can run it, that returns what it was given.
.echo_parser <- function(
  rep_dir,
  pixel_area_ha,
  by_class = FALSE,
  engine_rule = FALSE,
  event_fields = FALSE
) {
  list(
    rep_dir = rep_dir,
    area = pixel_area_ha,
    by_class = by_class,
    engine_rule = engine_rule,
    event_fields = event_fields
  )
}
environment(.echo_parser) <- baseenv()

.opts <- list(by_class = TRUE, engine_rule = TRUE, event_fields = TRUE)

test_that(".parse_rep_isolated() hands parse_options to the parser in the child", {
  expect_identical(
    .parse_rep_isolated(
      "somewhere",
      4,
      timeout_sec = 60,
      parser = .echo_parser,
      parse_options = .opts[-2]
    ),
    list(rep_dir = "somewhere", area = 4, by_class = TRUE, engine_rule = FALSE, event_fields = TRUE)
  )
  ## A parser that knows no options works with the default.
  plain <- function(rep_dir, pixel_area_ha) list(rep_dir, pixel_area_ha)
  environment(plain) <- baseenv()
  expect_identical(
    .parse_rep_isolated("somewhere", 4, timeout_sec = 60, parser = plain),
    list("somewhere", 4)
  )
})

test_that("sim_landis() hands parse_options to both parse paths; the default sends none", {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    patch_fire_config = function(...) invisible(NULL),
    landis_replicate = function(...) invisible(NULL),
    landis_pool_exec = function(...) invisible(NULL),
    .parse_rep_isolated = function(rep_dir, pixel_area_ha, timeout_sec, parse_options, ...) {
      seen$isolated <- parse_options
      list(n_events = 0L)
    },
    parse_dynamic_fire_logs = function(rep_dir, pixel_area_ha = 1, ...) {
      seen$in_process <- list(...)
      list(n_events = 0L)
    }
  )
  tmpl <- withr::local_tempdir()
  file.create(fs::path(tmpl, "dynamic-fire.txt"))
  writeLines("CellLength    100", fs::path(tmpl, "scenario.txt"))
  scratch <- withr::local_tempdir()
  fake_pool <- structure(list(names = "c1", scratch_root = scratch), class = "landis_pool")
  run <- function(...) {
    sim_landis(
      par_vec = c(NumFires = 1),
      paths = list(scenario_template = tmpl, scratch_root = scratch),
      sim_years = 1L,
      base_seed = 1L,
      pool = fake_pool,
      pool_idx = 1L,
      ...
    )
  }
  run(parse_timeout_sec = 5, parse_options = .opts)
  expect_identical(seen$isolated, .opts)
  run(parse_timeout_sec = NULL, parse_options = .opts)
  expect_identical(seen$in_process, .opts)

  seen$isolated <- seen$in_process <- NULL
  run(parse_timeout_sec = 5)
  expect_identical(seen$isolated, list())
  run(parse_timeout_sec = NULL)
  expect_identical(seen$in_process, list())
})

test_that("run_calibration_validation() hands parse_options to every replicate", {
  seen <- new.env(parent = emptyenv())
  scratch <- withr::local_tempdir()
  obs <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(pixel_area_ha = 1), obs)
  local_mocked_bindings(
    landis_pool_start = function(...) {
      list(scratch_root = scratch, image = "img", digest = "dig")
    },
    landis_pool_stop = function(...) invisible(NULL),
    sim_landis = function(..., base_seed, parse_options = "not sent") {
      seen$base_seed <- base_seed
      seen$parse_options <- parse_options
      list(n_events = 0L)
    },
    loss_from_stats = function(...) 0
  )
  run <- function(...) {
    run_calibration_validation(
      scenario_template = fs::path(scratch, "scenario.txt"),
      best_params = c(NumFires = 1),
      observed_targets_path = obs,
      cfg = list(sim_years = 1L),
      n_reps = 1L,
      scratch_root = scratch,
      ...
    )
  }
  run(parse_options = .opts)
  expect_identical(seen$parse_options, .opts)
  expect_identical(seen$base_seed, 100000L)
  expect_identical(eval(formals(run_calibration_validation)$base_seed), 99999L)
  seen$parse_options <- NULL
  run()
  expect_identical(seen$parse_options, list())
})

test_that("a parse_options that is not a list of the three switches is refused, naming it", {
  d <- .parse_logs()
  scratch <- withr::local_tempdir()
  obs <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(pixel_area_ha = 1), obs)
  started <- FALSE
  local_mocked_bindings(landis_pool_start = function(...) {
    started <<- TRUE
    stop("pool started", call. = FALSE)
  })
  bad <- list(
    list(by_klass = TRUE),
    list(by_class = "yes"),
    list(by_class = NA),
    list(by_class = c(TRUE, FALSE)),
    list(event_fields = "yes"),
    list(TRUE),
    list(by_class = TRUE, by_class = FALSE),
    c(by_class = TRUE)
  )
  for (b in bad) {
    expect_error(.check_parse_options(b), "parse_options")
    expect_error(
      .parse_rep_isolated("somewhere", 4, timeout_sec = 60, parse_options = b),
      "parse_options"
    )
    expect_error(
      sim_landis(
        par_vec = c(NumFires = 1),
        paths = list(scenario_template = d),
        sim_years = 1L,
        base_seed = 1L,
        parse_options = b
      ),
      "parse_options"
    )
    expect_error(
      run_calibration_validation(
        scenario_template = fs::path(scratch, "scenario.txt"),
        best_params = c(NumFires = 1),
        observed_targets_path = obs,
        cfg = list(sim_years = 1L),
        n_reps = 1L,
        scratch_root = scratch,
        parse_options = b
      ),
      "parse_options"
    )
  }
  expect_false(started)
  ## The offender is named.
  expect_error(.check_parse_options(list(by_klass = TRUE)), "by_klass")
  expect_error(.check_parse_options(list(by_class = "yes")), "by_class")
  expect_error(.check_parse_options(list(by_class = NA)), "by_class")
  expect_error(.check_parse_options(list(by_class = c(TRUE, FALSE))), "by_class")
  expect_error(.check_parse_options(list(event_fields = "yes")), "event_fields")
  expect_error(.check_parse_options(list(TRUE)), "name")
  expect_error(.check_parse_options(list(by_class = TRUE, by_class = FALSE)), "by_class")
  expect_error(.check_parse_options(c(by_class = TRUE)), "list")
  ## Valid ones pass through, invisibly.
  expect_identical(.check_parse_options(list()), list())
  expect_identical(.check_parse_options(.opts), .opts)
  expect_identical(.check_parse_options(list(event_fields = FALSE)), list(event_fields = FALSE))
})
