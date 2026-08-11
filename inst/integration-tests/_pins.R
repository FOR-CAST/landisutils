## Shared upstream-source pins for the integration-test harness. Sourced
## by build_scenarios.R and install_landis_windows.R. Keep all upstream-pin
## constants here so bumping the snapshot upstream-source is one edit.
##
## Conventions:
##   * `*_REPO` is `owner/name` on github.com.
##   * `*_REF` is either a 40-char SHA or a branch/tag name. Prefer SHAs
##     for reproducibility (build_scenarios.R needs a stable tarball).
##   * Tests/extension-list YAMLs are fetched from raw.githubusercontent.com
##     at the pinned SHA.

## Tool-Docker-Apptainer is the source of truth for which extensions ship
## in each docker image (release vs UCL v2). Same SHA also pins the
## upstream test-input tarballs (tests/Test*/inputs/...).
TDA_REPO <- "LANDIS-II-Foundation/Tool-Docker-Apptainer"
TDA_REF <- "043b43e6291813d5092721d45ff630502e777e30" ## 2026-08-03
## Bumped from 6a546fb (2026-04-29). Two things move with it:
##   * The UCLv2 list grows from 16 extensions to 20, gaining PnET-Succession,
##     Output-Biomass-PnET, Dynamic-Fire-System and Dynamic-Biomass-Fuels.
##   * `tests/TestNECN_UCLv2_AllExtension` is gone. Upstream stopped duplicating a whole test
##     directory per cohort generation and now keeps one set of inputs plus an extra scenario file
##     (`TestPnET_AllExtension/scenario_UCLv2.txt`). Nothing here consumed upstream's scenario.txt --
##     `build_scenarios.R` generates its own -- so the NECN builders simply repoint at
##     `tests/TestNECN_AllExtension/inputs`, whose file list is identical to the retired directory's.

## ForCS lives outside Tool-Docker-Apptainer; pinned to the same commit
## that the landis-ii-v8-release docker image references.
FORCS_REPO <- "LANDIS-II-Foundation/Extension-ForCS-Succession"
FORCS_REF <- "b761895100a7b30174dd78523d57cc63c592c887" ## 2026-05

## Core console Windows installer. WiX-built MSI checked into master of
## Core-Model-v8. Used by install_landis_windows.R.
CORE_MSI_REPO <- "LANDIS-II-Foundation/Core-Model-v8"
CORE_MSI_REF <- "master"
CORE_MSI_PATH <- "deploy/installer/en-us/LANDIS-II-8.0-setup64.msi"
CORE_MSI_URL <- sprintf(
  "https://github.com/%s/raw/%s/%s",
  CORE_MSI_REPO,
  CORE_MSI_REF,
  CORE_MSI_PATH
)
