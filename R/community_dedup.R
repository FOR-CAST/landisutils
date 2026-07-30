utils::globalVariables(c("MapCode", ".sig", ".new"))

#' Collapse duplicate communities in an Output Biomass Community snapshot
#'
#' Biomass Succession writes its Output Biomass Community state with **one map
#' code per pixel**: succession is evaluated per cell, so the writer emits a
#' distinct `MapCode` for every active cell and never re-collapses cells whose
#' cohort lists ended up identical. On a large landscape almost all of those map
#' codes are duplicates of one another, and the redundancy is carried entirely by
#' the CSV.
#'
#' That matters because LANDIS-II reads initial communities back through
#' `Landis.Library.InitialCommunities.Universal.DatasetParser`, which builds a
#' `System.Dynamic.ExpandoObject` per row. The parser's memory cost is a large
#' multiple of the file size, so a snapshot that is mostly duplicate rows can
#' exhaust a container's `--memory` and abort the run with
#' `System.OutOfMemoryException` inside `ReadCSVInputFile`, before the simulation
#' starts. Measured on a 2.98M-active-cell landscape: 2,684,154 map codes
#' carrying only 4,153 distinct communities -- a 1,472 MB CSV that deduplicates
#' to roughly 2 MB.
#'
#' This rewrites the pair so that one map code represents each **distinct**
#' community and the raster points every pixel at its community's new code. The
#' simulated state is unchanged: every pixel still maps to exactly the cohort
#' list it had before.
#'
#' Two communities are the same when their cohort sets are identical, compared
#' on every non-`MapCode` column after ordering rows canonically within a map
#' code. Biomass is compared exactly -- no rounding -- so this never merges
#' cells that differ, only cells that are already identical.
#'
#' @param csv Path to the snapshot CSV (`community-input-file-<t>.csv`), with a
#'   `MapCode` column plus the cohort columns (typically `SpeciesName`,
#'   `CohortAge`, `CohortBiomass`).
#' @param tif Path to the matching map-code raster (`output-community-<t>.tif`).
#' @param out_csv,out_tif Output paths. Default to overwriting `csv` / `tif`.
#' @param quiet Suppress the summary message.
#'
#' A snapshot can also contain **empty communities**: active cells whose map
#' code has no CSV rows at all (no cohorts -- recently disturbed, or active but
#' unforested). Biomass Succession emits these itself. They are active landscape,
#' so they are collapsed to one shared code and left active rather than zeroed;
#' the active-cell count is asserted unchanged before anything is written.
#'
#' @return Invisibly, a list with `csv`, `tif`, `map_codes_before`,
#'   `map_codes_after`, `rows_before`, `rows_after`, `empty_code` (the shared
#'   empty-community code, `NA` if there were none) and `empty_cells`.
#'
#' @family Dynamic Fire calibration helpers
#' @export
#' @seealso [run_calibration_spinup()]
dedup_community_snapshot <- function(csv, tif, out_csv = csv, out_tif = tif, quiet = FALSE) {
  stopifnot(
    requireNamespace("data.table", quietly = TRUE),
    requireNamespace("terra", quietly = TRUE),
    file.exists(csv),
    file.exists(tif)
  )

  d <- data.table::fread(csv, fill = TRUE, showProgress = FALSE)
  if (!("MapCode" %in% names(d))) {
    stop("`csv` has no MapCode column: ", csv, call. = FALSE)
  }
  cohort_cols <- setdiff(names(d), "MapCode")
  if (!length(cohort_cols)) {
    stop("`csv` has no cohort columns besides MapCode: ", csv, call. = FALSE)
  }
  rows_before <- nrow(d)

  ## Canonical row order WITHIN each map code, so two communities holding the same cohorts in a
  ## different order compare equal. Ordering by every cohort column (not just species) keeps the
  ## comparison total.
  data.table::setorderv(d, c("MapCode", cohort_cols))

  ## Signature per map code. Collapsing the cohort columns to one string per map code is what makes
  ## the comparison exact and order-independent in one pass.
  sig <- d[,
    list(.sig = paste(do.call(paste, c(.SD, sep = "\r")), collapse = "\n")),
    by = "MapCode",
    .SDcols = cohort_cols
  ]
  codes_before <- nrow(sig)

  ## New code per DISTINCT community, numbered from 1. `match()` on the signature maps every old code
  ## onto the first map code that carried that community.
  sig[, .new := match(.sig, unique(.sig))]
  codes_after <- data.table::uniqueN(sig$.new)

  ## Keep one representative map code's rows per distinct community, renumbered.
  rep_code <- sig[!duplicated(sig$.new), list(MapCode, .new)]
  out <- d[rep_code, on = "MapCode", nomatch = NULL]
  out[, MapCode := .new]
  out[, .new := NULL]
  data.table::setcolorder(out, c("MapCode", cohort_cols))
  data.table::setorderv(out, c("MapCode", cohort_cols))

  ## Remap the raster: old code -> new code. Cells outside the active landscape (0 / NA, the LANDIS-II
  ## inactive convention) must survive untouched, so substitute only the codes we actually renumbered.
  r <- terra::rast(tif)
  r2 <- terra::subst(r, from = sig$MapCode, to = sig$.new, others = NA)

  ## EMPTY COMMUNITIES. A snapshot legitimately contains active cells whose map code has no CSV rows:
  ## a cell with no cohorts (recently disturbed, or active but unforested). Biomass Succession emits
  ## these itself -- the NRD_Quesnel spinup had 95,063 of them. They are active landscape and must stay
  ## active, so they collapse to ONE shared code rather than being zeroed; zeroing them would silently
  ## shrink the simulated landscape (by 3.4% in that case). That shared code deliberately has no rows
  ## in the CSV, exactly as the codes it replaces had none.
  empty_code <- NA_integer_
  active_before <- !is.na(r) & r > 0
  unmapped <- active_before & is.na(r2)
  n_empty <- as.numeric(terra::global(unmapped, "sum", na.rm = TRUE)[1, 1])
  if (isTRUE(n_empty > 0)) {
    empty_code <- codes_after + 1L
    r2[unmapped] <- empty_code
  }
  ## anything still unmapped was inactive to begin with: restore the 0 / NA it originally carried
  r2 <- terra::cover(r2, r)
  names(r2) <- names(r)

  ## Verify before writing: every code left in the raster must resolve in the CSV (or be the shared
  ## empty-community code), or LANDIS-II aborts on load with "Unknown map code".
  present <- terra::unique(r2)[[1]]
  present <- present[!is.na(present) & present > 0]
  missing <- setdiff(present, c(out$MapCode, empty_code))
  if (length(missing)) {
    stop(
      "dedup produced ",
      length(missing),
      " raster map code(s) with no CSV rows (e.g. ",
      paste(utils::head(missing, 5), collapse = ", "),
      "); refusing to write",
      call. = FALSE
    )
  }
  ## This function renumbers; it must never reshape the landscape.
  n_before <- as.numeric(terra::global(active_before, "sum", na.rm = TRUE)[1, 1])
  n_after <- as.numeric(terra::global(!is.na(r2) & r2 > 0, "sum", na.rm = TRUE)[1, 1])
  if (!isTRUE(n_before == n_after)) {
    stop(
      "dedup changed the active-cell count (",
      format(n_before, big.mark = ","),
      " -> ",
      format(n_after, big.mark = ","),
      "); refusing to write",
      call. = FALSE
    )
  }

  size_before <- file.size(csv) ## capture BEFORE writing: out_csv defaults to csv
  data.table::fwrite(out, out_csv)
  terra::writeRaster(r2, out_tif, datatype = "INT4U", overwrite = TRUE)

  if (!quiet) {
    message(sprintf(
      paste0(
        "dedup_community_snapshot: %s -> %s map codes (%.1fx), %s -> %s rows; ",
        "CSV %.0f MB -> %.1f MB%s"
      ),
      format(codes_before, big.mark = ","),
      format(codes_after, big.mark = ","),
      codes_before / max(1L, codes_after),
      format(rows_before, big.mark = ","),
      format(nrow(out), big.mark = ","),
      size_before / 1048576,
      file.size(out_csv) / 1048576,
      if (!is.na(empty_code)) {
        sprintf(
          "; %s empty-community cell(s) -> code %d",
          format(n_empty, big.mark = ","),
          empty_code
        )
      } else {
        ""
      }
    ))
  }

  invisible(list(
    csv = as.character(out_csv),
    tif = as.character(out_tif),
    map_codes_before = codes_before,
    map_codes_after = codes_after,
    rows_before = rows_before,
    rows_after = nrow(out),
    empty_code = empty_code,
    empty_cells = if (is.na(empty_code)) 0 else n_empty
  ))
}
