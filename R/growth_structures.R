## Structure-factorial reduction and reporting.
##
## `growth_structure_design()` and `growth_calibration_partition()` BUILD the batched
## structure design; these read it back. Keeping the two halves apart is what let the
## `map_code` renumbering below be a trap rather than an invariant, so they belong in
## the same package.

#' Read a landscape cohort-structure table
#'
#' Reads the table written from [extract_landscape_cohort_structures()], which
#' enumerates the cohort structures a landscape's initial communities actually
#' contain: one row per (structure, cohort).
#'
#' @param path Character. Path to `landscape_cohort_structures.csv`.
#'
#' @return A tibble with `structure_id`, `n_cohorts`, `species`, `cohort_age` and
#'   `n_communities`.
#' @family growth calibration helpers
#' @export
read_landscape_cohort_structures <- function(path) {
  stopifnot(file.exists(path))
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      structure_id = as.character(.data$structure_id),
      n_cohorts = as.integer(.data$n_cohorts),
      species = as.character(.data$species),
      cohort_age = as.integer(.data$cohort_age),
      n_communities = as.integer(.data$n_communities)
    )
}

#' Reduce structure-factorial curves to one trajectory per cell
#'
#' A structure run's curves carry one row per (cell, timestep, COHORT), because
#' the design they are joined against is per cohort. The biomass column is a
#' WHOLE-CELL total either way, so a two-cohort cell arrives with its trajectory
#' repeated and any naive aggregation counts it twice.
#'
#' This reduces to one row per (cell, timestep) and attaches the cell's
#' composition: how many cohorts it carries and which modelled species, as a
#' sorted `+`-separated label so that `A+B` and `B+A` are the same mixture.
#'
#' @param curves A tibble of structure-run curves, carrying at least `batch`,
#'   `map_code`, `species`, `cohort_age`, `age` and `aboveground_c_mg_ha`. An
#'   optional `variant` column is carried through.
#'
#' @return A tibble with `batch`, `map_code`, `age`, `aboveground_c_mg_ha`,
#'   `n_cohorts` and `composition`.
#' @family growth calibration helpers
#' @export
growth_structure_cell_curves <- function(curves) {
  ## A cell is (batch, map_code), NOT map_code. `growth_calibration_partition()`
  ## renumbers map_code from 1 within each batch, so map_code 1 exists in every
  ## batch and grouping on it alone silently merges one cell per batch into a
  ## single fictional stand -- 21 cohorts on a design whose structures never hold
  ## more than 2. A single-batch smoke test cannot surface this.
  stopifnot("batch" %in% names(curves))
  ## `variant` is a function of `batch`, so adding it to the key changes no
  ## grouping -- it only keeps the column, which the distinct()s would otherwise
  ## drop and every comparison downstream needs.
  key <- c("batch", intersect("variant", names(curves)), "map_code")

  comp <- curves |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c(key, "species", "cohort_age")))) |>
    dplyr::summarise(
      n_cohorts = dplyr::n(),
      ## Duplicates kept: two cohorts of the SAME species at different ages is
      ## `Hw+Hw`, not `Hw`. Collapsing to unique species would label it
      ## identically to a single-cohort cell, which is a different experiment.
      composition = paste(sort(.data$species), collapse = "+"),
      .by = dplyr::all_of(key)
    )
  curves |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c(key, "age", "aboveground_c_mg_ha")))) |>
    dplyr::inner_join(comp, by = key) |>
    dplyr::arrange(.data$batch, .data$map_code, .data$age)
}

#' Summarise structure-factorial trajectories by stand composition
#'
#' Each cell is one (cohort structure, parameter combination) pair, so several
#' cells share a composition and differ in parameters and cohort ages. The
#' spread across them is therefore real variation in what the parameter set does
#' to that mixture, not noise, and is reported as a band rather than averaged
#' away.
#'
#' @param cells A tibble from [growth_structure_cell_curves()].
#' @param min_cells Integer. Drop compositions represented by fewer cells than
#'   this, which are too thin to read a band from.
#'
#' @return A tibble, one row per composition and age.
#' @family growth calibration helpers
#' @export
growth_structure_summary <- function(cells, min_cells = 25L) {
  ## Summarise WITHIN variant when the runs carry one. Pooling variants would
  ## average away the very difference the second set exists to show, and would do
  ## it silently -- the band would just widen.
  variant <- intersect("variant", names(cells))
  by_comp <- c("composition", variant)

  keep <- cells |>
    dplyr::summarise(
      n_cells = dplyr::n_distinct(paste(.data$batch, .data$map_code)),
      .by = dplyr::all_of(by_comp)
    ) |>
    dplyr::filter(.data$n_cells >= min_cells)

  cells |>
    dplyr::semi_join(keep, by = by_comp) |>
    dplyr::summarise(
      n_cells = dplyr::n_distinct(paste(.data$batch, .data$map_code)),
      lower = stats::quantile(.data$aboveground_c_mg_ha, 0.25, names = FALSE),
      median = stats::median(.data$aboveground_c_mg_ha),
      upper = stats::quantile(.data$aboveground_c_mg_ha, 0.75, names = FALSE),
      .by = dplyr::all_of(c(by_comp, "n_cohorts", "age"))
    ) |>
    dplyr::arrange(.data$composition, .data$age)
}

#' Tabulate the cohorts the structure factorial actually simulated
#'
#' What was run, rather than what it produced: one row per composition, with the
#' cells behind it, the cohort ages it started from, and where its carbon ends
#' up. This is the companion to the trajectory figures, which show shape but not
#' how much evidence sits under each panel or what the stands were.
#'
#' Reported per variant where the runs carry one, since the variants are
#' different experiments over the same design.
#'
#' `age_start` and `age_end` are SIMULATION years, and the cohort ages are the
#' ages the initial-communities map gave those cohorts at year 0 -- so a
#' composition can begin the run already near its plateau.
#'
#' @param cells A tibble from [growth_structure_cell_curves()].
#' @param curves Optional tibble as passed to [growth_structure_cell_curves()],
#'   carrying `species` and `cohort_age`. When given, the cohort-age range of
#'   each composition is reported; without it those columns are omitted.
#'
#' @return A tibble, one row per composition (and variant).
#' @family growth calibration helpers
#' @export
growth_structure_cohort_table <- function(cells, curves = NULL) {
  variant <- intersect("variant", names(cells))
  by_comp <- c(variant, "composition", "n_cohorts")

  out <- cells |>
    dplyr::summarise(
      n_cells = dplyr::n_distinct(paste(.data$batch, .data$map_code)),
      age_start = min(.data$age),
      age_end = max(.data$age),
      c_start = stats::median(.data$aboveground_c_mg_ha[.data$age == min(.data$age)]),
      c_peak = max(.data$aboveground_c_mg_ha),
      c_end = stats::median(.data$aboveground_c_mg_ha[.data$age == max(.data$age)]),
      .by = dplyr::all_of(by_comp)
    )

  if (!is.null(curves) && all(c("cohort_age", "map_code", "batch") %in% names(curves))) {
    ## Cohort ages are a property of the CELL at year 0, so take them from the
    ## distinct (cell, species, age) rows rather than from the per-timestep
    ## trajectory, which repeats each cohort once per year.
    key <- c("batch", variant, "map_code")
    ages <- curves |>
      dplyr::distinct(dplyr::pick(dplyr::all_of(c(key, "species", "cohort_age")))) |>
      dplyr::inner_join(
        dplyr::distinct(cells, dplyr::pick(dplyr::all_of(c(key, "composition")))),
        by = key
      ) |>
      dplyr::summarise(
        cohort_age_min = min(.data$cohort_age),
        cohort_age_median = stats::median(.data$cohort_age),
        cohort_age_max = max(.data$cohort_age),
        .by = dplyr::all_of(c(variant, "composition"))
      )
    out <- dplyr::left_join(out, ages, by = c(variant, "composition"))
  }

  dplyr::arrange(out, dplyr::desc(.data$n_cells), .data$composition)
}

.growth_structure_kinds <- c(
  "single cohort" = "#1b7837",
  "two cohorts, one species" = "#e08214",
  "two species" = "#762a83"
)

#' Plot mixtures against monocultures for one species
#'
#' Shows what sharing a cell does to total aboveground carbon: the species alone
#' against every two-cohort mixture it appears in. This is the question the
#' single-species calibration cells cannot answer, since they never let two
#' cohorts compete for light.
#'
#' The x axis is SIMULATION YEAR, not stand age. Unlike the verification runs,
#' which plant a single age-1 cohort, these cells start at the cohort ages the
#' initial-communities map actually carries, which can be several centuries. A
#' panel therefore begins at whatever carbon those cohorts already hold, and a
#' mid-run collapse is usually an old cohort reaching `longevity` rather than
#' anything the parameters did.
#'
#' @param summary A tibble from [growth_structure_summary()].
#' @param species Character. Modelled species code to plot.
#' @param x_max Numeric or `NULL`. Clip the data to this simulation year.
#'
#' @return A ggplot, or `NULL` when the species appears in no composition.
#' @family growth calibration helpers
#' @export
plot_growth_structures <- function(summary, species, x_max = 100) {
  .need("ggplot2", "Plotting stand structures")
  d <- dplyr::filter(summary, grepl(paste0("(^|\\+)", species, "($|\\+)"), .data$composition))
  if (nrow(d) == 0L) {
    return(NULL)
  }
  ## Clip the DATA, not just the axis: the panels are free-scaled on y, and a
  ## coord_cartesian zoom would leave every y range set by carbon the reader
  ## cannot see -- including the senescence collapse this horizon exists to
  ## simulate but production never reaches.
  if (!is.null(x_max)) {
    d <- dplyr::filter(d, .data$age <= x_max)
  }
  ## Three cases, not two: a second cohort of the SAME species is an age
  ## structure, not a mixture, and saying otherwise misreads the panel.
  has_variant <- "variant" %in% names(d) && dplyr::n_distinct(d$variant) > 1L
  d <- dplyr::mutate(
    d,
    n_species = lengths(lapply(strsplit(.data$composition, "+", fixed = TRUE), unique)),
    kind = dplyr::case_when(
      .data$n_cohorts == 1L ~ "single cohort",
      .data$n_species == 1L ~ "two cohorts, one species",
      .default = "two species"
    ),
    ## Built from the composition and the LARGEST cell count, not from each
    ## row's own: the variants share a design, so letting a per-variant count
    ## into the label would split one composition across two facets and destroy
    ## the comparison it exists to make.
    label = paste0(.data$composition, "  (", max(.data$n_cells), " cells)"),
    .by = "composition"
  )
  ## Ribbons only without a comparison. Two overlapping translucent bands per
  ## panel read as a third colour and hide both.
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$age))
  if (!has_variant) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper, fill = .data$kind),
        alpha = 0.25
      )
  }
  p +
    ggplot2::geom_line(
      if (has_variant) {
        ggplot2::aes(y = .data$median, colour = .data$kind, linetype = .data$variant)
      } else {
        ggplot2::aes(y = .data$median, colour = .data$kind)
      },
      linewidth = 0.8
    ) +
    ggplot2::facet_wrap(~label) +
    ggplot2::scale_colour_manual(values = .growth_structure_kinds) +
    ggplot2::scale_fill_manual(values = .growth_structure_kinds) +
    ggplot2::labs(
      title = paste0(species, ": stand composition and total cell carbon"),
      ## Two lines: on one it runs past the panel width and is silently clipped.
      subtitle = paste0(
        "median and interquartile range over the cells sharing each composition\n",
        "cohorts start at the ages the landscape gives them, so year 0 is not stand age 0",
        if (is.null(x_max)) {
          ""
        } else {
          paste0("\nshowing the first ", x_max, " yr; the run itself is longer")
        }
      ),
      x = "Simulation year",
      y = expression("Total aboveground live carbon (Mg C ha"^-1 * ")"),
      colour = NULL,
      fill = NULL,
      ## Only when a linetype is actually mapped: labelling an aesthetic no layer
      ## uses makes ggplot2 warn "Ignoring unknown labels" on every build.
      linetype = if (has_variant) "Variant"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
