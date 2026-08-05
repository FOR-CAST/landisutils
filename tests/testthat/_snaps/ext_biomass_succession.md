# prepMinRelativeBiomass rejects an unexpected schema

    Code
      prepMinRelativeBiomass(data.frame(ecoregion = 1L, shade1 = 0.15))
    Condition
      Error:
      ! prepMinRelativeBiomass(): `df` must have columns ecoregionGroup, X1, X2, X3, X4, X5 (one row per ecoregion); got: ecoregion, shade1

