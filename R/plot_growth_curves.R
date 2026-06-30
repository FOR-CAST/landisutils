## Biomass Succession calibration diagnostic: fitted growth curves vs PSP data ------------------------

#' Plot fitted LANDIS-II growth curves against PSP observations
#'
#' Overlays the fitted LANDIS-version growth curve (`BscaledNonLinear`) on the
#' permanent-sample-plot (PSP) biomass observations behind it, faceted by
#' species and coloured by ecoregion. A calibration diagnostic for the Biomass
#' Succession species parameterization produced -- in LANDIS mode -- by the
#' `Biomass_speciesParameters` SpaDES module: it shows how each fitted growth
#' curve sits within the spread of the PSP data, and how that data is
#' distributed across ecoregions.
#'
#' Only species present in `landis_curves` get a fitted line; species that have
#' PSP observations but no fitted curve (e.g. data-poor species, or a lumped
#' `"Other"` group) are still shown as points.
#'
#' @param landis_curves A data frame with columns `species`, `standAge`, and
#'   `BscaledNonLinear` -- e.g. the `speciesGrowthCurvesLandis` output of
#'   `Biomass_speciesParameters` run in LANDIS mode.
#' @param psp_points A data frame with columns `speciesTemp` (species code),
#'   `standAge`, `biomass`, and `ecoregion` -- e.g. the `speciesGrowthCurvesPSP`
#'   output of `Biomass_speciesParameters` run in LANDIS mode.
#' @param biomass_label Axis label for biomass, as a plotmath expression (or a
#'   plain string). Defaults to `quote("Biomass" ~ (g ~ m^-2))`.
#'
#' @returns A `ggplot` object.
#'
#' @export
plot_species_growth_curves <- function(
  landis_curves,
  psp_points,
  biomass_label = quote("Biomass" ~ (g ~ m^-2))
) {
  stopifnot(
    all(c("species", "standAge", "BscaledNonLinear") %in% names(landis_curves)),
    all(c("speciesTemp", "standAge", "biomass", "ecoregion") %in% names(psp_points))
  )

  ## facet on a common `species` column (PSP carries it as `speciesTemp`)
  psp_points[["species"]] <- psp_points[["speciesTemp"]]

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = psp_points,
      ggplot2::aes(x = .data$standAge, y = .data$biomass, colour = .data$ecoregion),
      alpha = 0.55,
      size = 1.1
    ) +
    ggplot2::geom_line(
      data = landis_curves,
      ggplot2::aes(x = .data$standAge, y = .data$BscaledNonLinear),
      linewidth = 1,
      colour = "grey15"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$species), scales = "free_y") +
    ggplot2::labs(x = "Stand age (years)", y = biomass_label, colour = "Ecoregion (BEC)") +
    ggplot2::theme_bw(base_size = 11)
}
