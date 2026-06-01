## Shared upstream-source pins for the integration-test harness. Sourced
## by build_scenarios.R, install_landis_windows.R, and install_landis_
## linux.sh (via R --vanilla -e 'source(...)'). Keep all upstream-pin
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
TDA_REF <- "6a546fbb55f722751f78925089784152378eebb0" ## 2026-04-29

## ForCS lives outside Tool-Docker-Apptainer; pinned to the same commit
## that the landis-ii-v8-release docker image references.
FORCS_REPO <- "LANDIS-II-Foundation/Extension-ForCS-Succession"
FORCS_REF <- "b761895100a7b30174dd78523d57cc63c592c887" ## 2026-05

## Core console Windows installer. WiX-built MSI checked into master of
## Core-Model-v8. Used by install_landis_windows.R; the Linux native
## install path extracts the runtime from the docker image instead.
CORE_MSI_REPO <- "LANDIS-II-Foundation/Core-Model-v8"
CORE_MSI_REF <- "master"
CORE_MSI_PATH <- "deploy/installer/en-us/LANDIS-II-8.0-setup64.msi"
CORE_MSI_URL <- sprintf(
  "https://github.com/%s/raw/%s/%s",
  CORE_MSI_REPO,
  CORE_MSI_REF,
  CORE_MSI_PATH
)

## Docker images used by run_in_docker.sh and as the source of the native
## LANDIS install on Linux runners (install_landis_linux.sh extracts the
## release image's /opt/landis-ii tree).
LANDIS_IMAGE_RELEASE <- "ghcr.io/landis-ii-foundation/landis-ii-v8-release:main"
LANDIS_IMAGE_UCLV2 <- "ghcr.io/landis-ii-foundation/landis-ii-v8-uclv2-release:main"
