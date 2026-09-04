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

## `Hw` for one cohort, `Hw x3` for three -- so that a single-cohort cell keeps
## the bare species code and the label stays short however many age classes a
## cell carries.
## Species present, ordered by the OLDEST cohort each contributes, descending.
## Alphabetical tiebreak, so two species starting at the same age give a
## deterministic order rather than one that depends on row order.
.growth_species_by_age <- function(species, age) {
  oldest <- vapply(split(age, species), max, numeric(1))
  names(oldest)[order(-oldest, names(oldest))]
}

.growth_composition_label <- function(species, age = NULL) {
  n <- table(species)
  ## Ordered oldest-first when ages are supplied, so `Hw+Ba` reads as "hemlock
  ## the older cohort, amabilis fir the younger" and is a DIFFERENT stand from
  ## `Ba+Hw`. Those two were previously collapsed by sorting alphabetically, and
  ## on this landscape they are not interchangeable: the `Ba+Hw` pool carried
  ## 14,555 communities against 1,256 for `Hw+Ba`, so the pooled panel was an
  ## average over two populations differing more than tenfold in extent.
  ## Falls back to alphabetical without ages, which is what a caller reducing a
  ## table that has none will get.
  nm <- if (is.null(age)) sort(names(n)) else .growth_species_by_age(species, age)
  paste(ifelse(n[nm] > 1L, paste0(nm, " x", as.integer(n[nm])), nm), collapse = "+")
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
#' `+`-separated label ordered OLDEST cohort first, so that `A+B` and `B+A`
#' are different mixtures rather than the same one.
#'
#' @param curves A tibble of structure-run curves, carrying at least `batch`,
#'   `map_code`, `species`, `cohort_age`, `age` and `aboveground_c_mg_ha`. An
#'   optional `variant` column is carried through.
#' @param biomass What `aboveground_c_mg_ha` MEANS, which differs by extension
#'   and cannot be detected from the data. `"cell"` (the default) is a whole-cell
#'   total already, repeated once per cohort by the join, and is de-duplicated.
#'   `"cohort"` is that cohort's own biomass and is SUMMED over the cell's
#'   cohorts. Getting this wrong is quiet: de-duplicating per-cohort values keeps
#'   one row per DISTINCT VALUE, which is neither a total nor a trajectory.
#'
#' @return A tibble with `batch`, `map_code`, `age`, `aboveground_c_mg_ha`,
#'   `n_cohorts`, `composition` (a display label, counted: `Hw` for one cohort,
#'   `Hw x3` for three), `species_set` (the species present, for matching) and
#'   `start_age` (the oldest cohort the cell begins with).
#' @family growth calibration helpers
#' @export
growth_structure_cell_curves <- function(curves, biomass = c("cell", "cohort")) {
  biomass <- match.arg(biomass)
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
      ## Repeats are NOT collapsed away: two cohorts of one species at different
      ## ages is a different experiment from one cohort of it, and labelling both
      ## `Hw` would merge them. They are COUNTED rather than repeated, because a
      ## structure design is capped on species and not cohorts -- repeating the
      ## name per cohort gives a 255-character label on a landscape whose cells
      ## carry a dozen age classes, which is unusable as a facet strip.
      composition = .growth_composition_label(.data$species, .data$cohort_age),
      ## Which species the cell's oldest cohort belongs to. This is what
      ## `plot_growth_structures()` selects a species' panels on, so that a
      ## structure appears in exactly ONE species' figure instead of once per
      ## species it contains.
      oldest_species = .growth_species_by_age(.data$species, .data$cohort_age)[[1L]],
      ## The species actually present, for matching and counting. `composition`
      ## is a display label and must not be parsed for this.
      species_set = paste(sort(unique(.data$species)), collapse = "+"),
      ## The oldest cohort the cell starts with, which is what dominates its
      ## carbon. Measured on the Gitanyow structure sweep, starting age explains
      ## 92 to 99 percent of the spread within a composition, against 0.2 to 3.3
      ## percent for the swept parameters. It therefore has to survive to the
      ## summary: pooling over it produces a band roughly ten times the
      ## parameter effect it is read as showing.
      start_age = max(.data$cohort_age),
      .by = dplyr::all_of(key)
    )
  per_cell <- if (biomass == "cell") {
    dplyr::distinct(curves, dplyr::pick(dplyr::all_of(c(key, "age", "aboveground_c_mg_ha"))))
  } else {
    dplyr::summarise(
      curves,
      aboveground_c_mg_ha = sum(.data$aboveground_c_mg_ha),
      .by = dplyr::all_of(c(key, "age"))
    )
  }

  per_cell |>
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
#' Pooling over starting age is usually the wrong default for reading a band.
#' A composition's cells differ in two ways at once: the parameter combination,
#' and the ages the map gave their cohorts. The second dominates, so a band
#' taken over the pool is mostly stand age wearing the parameters' name. Supply
#' `start_age_breaks` to summarise WITHIN starting-age classes instead, which
#' leaves the remaining spread attributable to the parameters.
#'
#' @param cells A tibble from [growth_structure_cell_curves()].
#' @param min_cells Integer. Drop compositions represented by fewer cells than
#'   this, which are too thin to read a band from. Applied per class when
#'   `start_age_breaks` is given, so a stratified call needs more cells overall.
#' @param start_age_breaks Numeric or `NULL`. Breaks passed to [cut()] to bin
#'   each cell's `start_age`, adding a `start_class` column and summarising
#'   within it. `NULL` (the default) pools over starting age, which is the
#'   historical behaviour. Requires `cells` to carry `start_age`.
#'
#' @return A tibble, one row per composition and age (and `start_class` when
#'   stratified).
#' @family growth calibration helpers
#' @export
growth_structure_summary <- function(cells, min_cells = 25L, start_age_breaks = NULL) {
  strat <- character(0)
  if (!is.null(start_age_breaks)) {
    if (!"start_age" %in% names(cells)) {
      stop(
        "`start_age_breaks` needs a `start_age` column. Rebuild `cells` with ",
        "growth_structure_cell_curves() from landisutils >= 0.0.140.",
        call. = FALSE
      )
    }
    cells <- dplyr::mutate(
      cells,
      start_class = cut(.data$start_age, breaks = start_age_breaks, dig.lab = 4L)
    )
    strat <- "start_class"
  }
  ## Summarise WITHIN variant when the runs carry one. Pooling variants would
  ## average away the very difference the second set exists to show, and would do
  ## it silently -- the band would just widen.
  variant <- intersect("variant", names(cells))
  ## `species_set` is a function of `composition`, so adding it changes no
  ## grouping -- it only keeps the column, which the plot needs to match species.
  by_comp <- c(
    "composition",
    intersect(c("species_set", "oldest_species"), names(cells)),
    variant,
    strat
  )

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
  by_comp <- c(
    variant,
    "composition",
    intersect(c("species_set", "oldest_species"), names(cells)),
    "n_cohorts"
  )

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

## Deliberately NOT phrased in terms of two: a structure design is capped on
## SPECIES, not cohorts, so a one-species cell can carry a dozen age classes and
## a label saying "two" would be wrong on most landscapes.
.growth_structure_kinds <- c(
  "single cohort" = "#1b7837",
  "one species, multiple cohorts" = "#e08214",
  "multiple species" = "#762a83"
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
#' @param max_panels Integer or `NULL`. Keep only this many compositions, those
#'   with the most cells behind them. `NULL` (the default) keeps all, which is
#'   right when cells hold at most a couple of cohorts and a species appears in
#'   a handful of compositions. Where cells carry many age classes a species can
#'   appear in over a hundred, and a facet per composition renders as unreadable
#'   slivers with truncated strips. What was dropped is stated in the subtitle
#'   rather than left implied.
#'
#' @return A ggplot, or `NULL` when the species appears in no composition.
#' @family growth calibration helpers
#' @export
plot_growth_structures <- function(summary, species, x_max = 100, max_panels = NULL) {
  .need("ggplot2", "Plotting stand structures")
  ## Selected on the OLDEST cohort's species, so each structure appears in
  ## exactly one species' figure and the focal species always leads its own
  ## panel labels. Selecting on mere presence put every mixture in both species'
  ## figures as the same panel, which read as a duplicate and hid that `Ba+Hw`
  ## and `Hw+Ba` are different stands.
  ##
  ## Falls back to matching the species SET -- never the display label, which
  ## carries counts and would not match a bare species code -- so a summary
  ## built before `oldest_species` existed still plots, with the old behaviour.
  d <- if ("oldest_species" %in% names(summary)) {
    dplyr::filter(summary, .data$oldest_species == species)
  } else {
    match_col <- if ("species_set" %in% names(summary)) "species_set" else "composition"
    dplyr::filter(summary, grepl(paste0("(^|\\+)", species, "($|\\+)"), .data[[match_col]]))
  }
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
  ## Keep the best-evidenced compositions. Ordered by cells first and then by
  ## name, so the selection is deterministic rather than dependent on row order.
  n_comp <- dplyr::n_distinct(d$composition)
  dropped <- 0L
  if (!is.null(max_panels) && n_comp > max_panels) {
    rank <- d |>
      dplyr::summarise(n_cells = max(.data$n_cells), .by = "composition") |>
      dplyr::arrange(dplyr::desc(.data$n_cells), .data$composition)
    d <- dplyr::filter(d, .data$composition %in% rank$composition[seq_len(max_panels)])
    dropped <- n_comp - max_panels
  }
  ## Three cases, not two: a second cohort of the SAME species is an age
  ## structure, not a mixture, and saying otherwise misreads the panel.
  has_variant <- "variant" %in% names(d) && dplyr::n_distinct(d$variant) > 1L
  ## Stratified summaries carry their own colour variable, and it is the one
  ## worth having: within a panel `kind` is CONSTANT (a panel is one
  ## composition), so it only ever restated the facet strip.
  has_strata <- "start_class" %in% names(d)
  ## With strata the per-row count is per class, so a panel label built from it
  ## would report the largest class rather than the composition.
  lab_n <- if (has_strata) {
    d |>
      dplyr::distinct(.data$composition, .data$start_class, .data$n_cells) |>
      dplyr::summarise(n = sum(.data$n_cells), .by = "composition")
  } else {
    ## The LARGEST cell count, not each row's own: the variants share a design,
    ## so letting a per-variant count into the label would split one composition
    ## across two facets and destroy the comparison it exists to make.
    dplyr::summarise(d, n = max(.data$n_cells), .by = "composition")
  }
  d <- d |>
    dplyr::mutate(
      n_species = lengths(lapply(strsplit(.data[[match_col]], "+", fixed = TRUE), unique)),
      kind = dplyr::case_when(
        .data$n_cohorts == 1L ~ "single cohort",
        .data$n_species == 1L ~ "one species, multiple cohorts",
        .default = "multiple species"
      ),
      .by = "composition"
    ) |>
    dplyr::inner_join(lab_n, by = "composition") |>
    dplyr::mutate(label = paste0(.data$composition, "  (", .data$n, " cells)"))
  ## Ribbons only without a comparison. Overlapping translucent bands read as a
  ## further colour and hide each other, which is true of two variants and much
  ## truer of seven or eight starting-age classes.
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$age))
  if (!has_variant && !has_strata) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper, fill = .data$kind),
        alpha = 0.25
      )
  }
  colour_var <- if (has_strata) "start_class" else "kind"
  p <- p +
    ggplot2::geom_line(
      if (has_variant) {
        ggplot2::aes(y = .data$median, colour = .data[[colour_var]], linetype = .data$variant)
      } else {
        ggplot2::aes(y = .data$median, colour = .data[[colour_var]])
      },
      linewidth = 0.8
    ) +
    ggplot2::facet_wrap(~label)
  p <- if (has_strata) {
    ## Ordered magnitude, so the ramp darkens with age: more is darker. Truncated
    ## at 0.75 because viridis ends in a yellow that sits at 1.23:1 against a
    ## light page and vanishes; 0.75 puts the light end at 2.06:1. Multi-hue is a
    ## deliberate departure from one-hue-sequential, taken because eight steps of
    ## a single hue are not separable at panel size -- lightness stays monotone,
    ## so the ordering survives in greyscale and under colour-vision deficiency,
    ## with hue as a redundant second channel. `drop = FALSE` keeps the legend
    ## identical across species, so the per-species figures stay comparable.
    p +
      ggplot2::scale_colour_viridis_d(
        end = 0.75,
        direction = -1,
        drop = FALSE,
        name = "Oldest cohort at year 0 (yr)"
      )
  } else {
    p +
      ggplot2::scale_colour_manual(values = .growth_structure_kinds) +
      ggplot2::scale_fill_manual(values = .growth_structure_kinds)
  }
  p +
    ggplot2::labs(
      title = paste0(species, ": stand composition and total cell carbon"),
      ## Two lines: on one it runs past the panel width and is silently clipped.
      subtitle = paste0(
        if (has_strata) {
          paste0(
            "median cell carbon within each starting-age class; the spacing ",
            "between lines is stand age, not parameters\n"
          )
        } else {
          "median and interquartile range over the cells sharing each composition\n"
        },
        "cohorts start at the ages the landscape gives them, so year 0 is not stand age 0",
        if (is.null(x_max)) {
          ""
        } else {
          paste0("\nshowing the first ", x_max, " yr; the run itself is longer")
        },
        if (dropped > 0L) {
          paste0("\nthe ", n_comp - dropped, " compositions with the most cells, of ", n_comp)
        } else {
          ""
        }
      ),
      x = "Simulation year",
      y = expression("Total aboveground live carbon (Mg C ha"^-1 * ")"),
      ## Matches the scale's own `name` when stratified rather than fighting it,
      ## so the legend title cannot depend on which of the two wins.
      colour = if (has_strata) "Oldest cohort at year 0 (yr)" else NULL,
      fill = NULL,
      ## Only when a linetype is actually mapped: labelling an aesthetic no layer
      ## uses makes ggplot2 warn "Ignoring unknown labels" on every build.
      linetype = if (has_variant) "Variant"
    ) +
    ## One row: eight classes wrapped over two rows push the panels up and cost
    ## more height than the legend saves.
    ggplot2::guides(
      colour = if (has_strata) ggplot2::guide_legend(nrow = 1) else ggplot2::waiver()
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
