# Social-Climate-Fire Extension

Social-Climate-Fire Extension

Social-Climate-Fire Extension

## References

LANDIS-II Social-Climate-Fire v4 User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire/blob/master/docs/LANDIS-II%20Social-Climate-Fire%20v4%20User%20Guide.pdf>

Scheller, R., Kretchun, A., Hawbaker, T.J. & Henne, P.D. (2019). A
landscape model of variable social-ecological fire regimes. Ecological
Modelling, 401, 85–93.
[doi:10.1016/j.ecolmodel.2019.03.022](https://doi.org/10.1016/j.ecolmodel.2019.03.022)

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `SocialClimateFire`

## Active bindings

- `TimeZeroPET`:

  (Optional) Real. Potential Evapotranspiration (PET) for time zero.

- `TimeZeroCWD`:

  (Optional) Real. Climate Water Deficit (CWD) for time zero.

- `Species_CSV_File`:

  Character. Relative file path.

- `AccidentalIgnitionsMap`:

  Character. Relative file path.

- `DynamicAccidentalIgnitionMaps`:

  (Optional) `data.frame` with columns `Year` and `FileName`.

- `LightningIgnitionsMap`:

  Character. Relative file path.

- `DynamicLightningIgnitionsMaps`:

  (Optional) `data.frame` with columns `Year` and `FileName`.

- `RxIgnitionsMap`:

  Character. Relative file path.

- `DynamicRxIgnitionsMaps`:

  (Optional) `data.frame` with columns `Year` and `FileName`.

- `AccidentalSuppressionMap`:

  Character. Relative file path.

- `LightningSuppressionMap`:

  Character. Relative file path.

- `RxSuppressionMap`:

  Character. Relative file path.

- `DynamicAccidentalSuppressionMaps`:

  (Optional) `data.frame` with columns `Year` and `FileName`.

- `GroundSlopeFile`:

  Character. Relative file path.

- `UphillSlopeAzimuthMap`:

  Character. Relative file path.

- `ClayMap`:

  Character. Relative file path.

- `LightningIgnitionsCoeffs`:

  Real. Parameters B0 and B1 from equation 1 (Scheller et al. 2019).

- `AccidentalIgnitionsCoeffs`:

  Real. B0 and B1 from equation 1 (Scheller et al. 2019).

- `IgnitionDistribution`:

  Character. One of "Poisson" or "ZeroInflatedPoisson".

- `LightningIgnitionsBinomialCoeffs`:

  Real. Parameters \$b_z_0\$ and \$b_z_1\$ from equation 2 (Scheller et
  al. 2019).

- `AccidentalIgnitionsBinomialCoeffs`:

  Real. Parameters \$b_z_0\$ and \$b_z_1\$ from equation 2 (Scheller et
  al. 2019).

- `MaximumFineFuels`:

  Real. Maximum amount of fine fuels (\$g/m^2\$).

- `MaximumRxWindSpeed`:

  Real. Maximum wind speed (\$m/s\$) for prescribed fires.

- `MaximumRxFireWeatherIndex`:

  (Optional) Real. Maximum Fire Weather Index (FWI) for prescribed
  fires.

- `MinimumRxFireWeatherIndex`:

  (Optional) Real. Minimum Fire Weather Index (FWI) for prescribed
  fires.

- `MaximumRxTemperature`:

  (Optional) Real. Maximum temperature for prescribed fires.

- `MinimumRxRelativeHumidity`:

  (Optional) Real. Minimum relative humidity for prescribed fires.

- `MaximumRXFireIntensity`:

  Integer. Maximum allowable fire intensity for prescribed fires.

- `NumberRxAnnualFires`:

  Integer. Number of prescribed fires attempted per year.

- `NumberRxDailyFires`:

  Integer. Number of prescribed fires attempted per day.

- `FirstDayRxFires`:

  Integer. First Julian day in which a prescribed fire can begin.

- `LastDayRxFires`:

  Integer. Last Julian day in which a prescribed fire can begin.

- `TargetRxSize`:

  Real. Maximum size for prescribed fire (\$ha\$).

- `RxZonesMap`:

  (Optional) Character. Relative file path.

- `MaximumSpreadAreaCoeffs`:

  Real. Parameters B0, B1, B2 from equation 4 (Scheller et al. 2019).

- `SpreadProbabilityCoeffs`:

  Real. Parameters B0, B1, B2 from equation 3 (Scheller et al. 2019).

- `SiteMortalityCoeffs`:

  Real. Parameters B0, B1, B2, B3, B4, B5, B6 from equation 7 (Scheller
  et al. 2019).

- `CohortMortalityCoeffs`:

  Real. Parameters B0, B1, B2 from equation 10 (Scheller et al. 2019).

- `LadderFuelMaxAge`:

  Integer. Maximum age for ladder fuels.

- `LadderFuelSpeciesList`:

  Character. Vector of species codes considered to be ladder fuels.

- `SuppressionMaxWindSpeed`:

  Real. Maximum wind speed (\$m/s\$) for fire suppression.

- `Suppression_CSV_File`:

  Character. Relative file path.

- `DeadWoodTable`:

  `data.frame` with columns `Species` and `Age`.

## Methods

### Public methods

- [`SocialClimateFire$new()`](#method-SocialClimateFire-new)

- [`SocialClimateFire$write()`](#method-SocialClimateFire-write)

- [`SocialClimateFire$clone()`](#method-SocialClimateFire-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    SocialClimateFire$new(
      path,
      Timestep = 1,
      TimeZeroPET = NULL,
      TimeZeroCWD = NULL,
      Species_CSV_File = NULL,
      AccidentalIgnitionsMap = NULL,
      DynamicAccidentalIgnitionMaps = NULL,
      LightningIgnitionsMap = NULL,
      DynamicLightningIgnitionsMaps = NULL,
      RxIgnitionsMap = NULL,
      DynamicRxIgnitionsMaps = NULL,
      AccidentalSuppressionMap = NULL,
      LightningSuppressionMap = NULL,
      RxSuppressionMap = NULL,
      DynamicAccidentalSuppressionMaps = NULL,
      GroundSlopeFile = NULL,
      UphillSlopeAzimuthMap = NULL,
      ClayMap = NULL,
      LightningIgnitionsCoeffs = NULL,
      AccidentalIgnitionsCoeffs = NULL,
      IgnitionDistribution = NULL,
      LightningIgnitionsBinomialCoeffs = NULL,
      AccidentalIgnitionsBinomialCoeffs = NULL,
      MaximumFineFuels = NULL,
      MaximumRxWindSpeed = NULL,
      MaximumRxFireWeatherIndex = NULL,
      MinimumRxFireWeatherIndex = NULL,
      MaximumRxTemperature = NULL,
      MinimumRxRelativeHumidity = NULL,
      MaximumRXFireIntensity = NULL,
      NumberRxAnnualFires = NULL,
      NumberRxDailyFires = NULL,
      FirstDayRxFires = NULL,
      LastDayRxFires = NULL,
      TargetRxSize = NULL,
      RxZonesMap = NULL,
      MaximumSpreadAreaCoeffs = NULL,
      SpreadProbabilityCoeffs = NULL,
      SiteMortalityCoeffs = NULL,
      CohortMortalityCoeffs = NULL,
      LadderFuelMaxAge = NULL,
      LadderFuelSpeciesList = NULL,
      SuppressionMaxWindSpeed = NULL,
      Suppression_CSV_File = NULL,
      DeadWoodTable = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Not used (see extension user guide).

- `TimeZeroPET`:

  (Optional) Real. Potential Evapotranspiration (PET) for time zero.

- `TimeZeroCWD`:

  (Optional) Real. Climate Water Deficit (CWD) for time zero.

- `Species_CSV_File`:

  Character. Relative file path.

- `AccidentalIgnitionsMap`:

  Character. Relative file path.

- `DynamicAccidentalIgnitionMaps`:

  (Optional) `data.frame`.

- `LightningIgnitionsMap`:

  Character. Relative file path.

- `DynamicLightningIgnitionsMaps`:

  (Optional) `data.frame`.

- `RxIgnitionsMap`:

  Character. Relative file path.

- `DynamicRxIgnitionsMaps`:

  (Optional) `data.frame`.

- `AccidentalSuppressionMap`:

  Character. Relative file path.

- `LightningSuppressionMap`:

  Character. Relative file path.

- `RxSuppressionMap`:

  Character. Relative file path.

- `DynamicAccidentalSuppressionMaps`:

  (Optional) `data.frame`.

- `GroundSlopeFile`:

  Character. Relative file path.

- `UphillSlopeAzimuthMap`:

  Character. Relative file path.

- `ClayMap`:

  Character. Relative file path.

- `LightningIgnitionsCoeffs`:

  Real. Parameters B0 and B1 from equation 1 (Scheller et al. 2019).

- `AccidentalIgnitionsCoeffs`:

  Real. B0 and B1 from equation 1 (Scheller et al. 2019).

- `IgnitionDistribution`:

  Character. One of `"Poisson"` or `"ZeroInflatedPoisson"`.

- `LightningIgnitionsBinomialCoeffs`:

  Real. Parameters \$b_z_0\$ and \$b_z_1\$ from equation 2 (Scheller et
  al. 2019).

- `AccidentalIgnitionsBinomialCoeffs`:

  Real. Parameters \$b_z_0\$ and \$b_z_1\$ from equation 2 (Scheller et
  al. 2019).

- `MaximumFineFuels`:

  Real. Maximum amount of fine fuels (\$g/m^2\$).

- `MaximumRxWindSpeed`:

  Real. Maximum wind speed (\$m/s\$) for prescribed fires.

- `MaximumRxFireWeatherIndex`:

  (Optional) Real. Maximum Fire Weather Index (FWI) for prescribed
  fires.

- `MinimumRxFireWeatherIndex`:

  (Optional) Real. Minimum Fire Weather Index (FWI) for prescribed
  fires.

- `MaximumRxTemperature`:

  (Optional) Real. Maximum temperature for prescribed fires.

- `MinimumRxRelativeHumidity`:

  (Optional) Real. Minimum relative humidity for prescribed fires.

- `MaximumRXFireIntensity`:

  Integer. Maximum allowable fire intensity for prescribed fires.

- `NumberRxAnnualFires`:

  Integer. Number of prescribed fires attempted per year.

- `NumberRxDailyFires`:

  Integer. Number of prescribed fires attempted per day.

- `FirstDayRxFires`:

  Integer. First Julian day in which a prescribed fire can begin.

- `LastDayRxFires`:

  Integer. Last Julian day in which a prescribed fire can begin.

- `TargetRxSize`:

  Real. Maximum size for prescribed fire (\$ha\$).

- `RxZonesMap`:

  (Optional) Character. Relative file path.

- `MaximumSpreadAreaCoeffs`:

  Real. Parameters B0, B1, B2 from equation 4 (Scheller et al. 2019).

- `SpreadProbabilityCoeffs`:

  Real. Parameters B0, B1, B2 from equation 3 (Scheller et al. 2019).

- `SiteMortalityCoeffs`:

  Real. Parameters B0, B1, B2, B3, B4, B5, B6 from equation 7 (Scheller
  et al. 2019).

- `CohortMortalityCoeffs`:

  Real. Parameters B0, B1, B2 from equation 10 (Scheller et al. 2019).

- `LadderFuelMaxAge`:

  Integer. Maximum age for ladder fuels.

- `LadderFuelSpeciesList`:

  Character. Vector of species codes considered to be ladder fuels.

- `SuppressionMaxWindSpeed`:

  Real. Maximum wind speed (\$m/s\$) for fire suppression.

- `Suppression_CSV_File`:

  Character. Relative file path.

- `DeadWoodTable`:

  `data.frame`.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    SocialClimateFire$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SocialClimateFire$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## see vignette for usage examples
```
