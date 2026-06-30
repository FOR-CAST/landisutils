# Plot fitted LANDIS-II growth curves against PSP observations

Overlays the fitted LANDIS-version growth curve (`BscaledNonLinear`) on
the permanent-sample-plot (PSP) biomass observations behind it, faceted
by species and coloured by ecoregion. A calibration diagnostic for the
Biomass Succession species parameterization produced – in LANDIS mode –
by the `Biomass_speciesParameters` SpaDES module: it shows how each
fitted growth curve sits within the spread of the PSP data, and how that
data is distributed across ecoregions.

## Usage

``` r
plot_species_growth_curves(
  landis_curves,
  psp_points,
  biomass_label = quote("Biomass" ~ (g ~ m^-2))
)
```

## Arguments

- landis_curves:

  A data frame with columns `species`, `standAge`, and
  `BscaledNonLinear` – e.g. the `speciesGrowthCurvesLandis` output of
  `Biomass_speciesParameters` run in LANDIS mode.

- psp_points:

  A data frame with columns `speciesTemp` (species code), `standAge`,
  `biomass`, and `ecoregion` – e.g. the `speciesGrowthCurvesPSP` output
  of `Biomass_speciesParameters` run in LANDIS mode.

- biomass_label:

  Axis label for biomass, as a plotmath expression (or a plain string).
  Defaults to `quote("Biomass" ~ (g ~ m^-2))`.

## Value

A `ggplot` object.

## Details

Only species present in `landis_curves` get a fitted line; species that
have PSP observations but no fitted curve (e.g. data-poor species, or a
lumped `"Other"` group) are still shown as points.
