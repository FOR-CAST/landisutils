# LANDIS-II integration tests

A parser-level harness that runs `landisutils`-generated scenario files
through the official LANDIS-II v8 console (in an ephemeral Docker
container) to validate the syntax of the package's writers, beyond what
`R CMD check` and `tests/testthat/` can catch.

## What's here

| File | Purpose |
| --- | --- |
| [`_pins.R`](_pins.R) | Shared upstream-source pins (Tool-Docker-Apptainer SHA, ForCS SHA, Core MSI URL, Docker image refs). Sourced by `build_scenarios.R` and `install_landis_windows.R`; read by `install_landis_linux.sh`. |
| [`build_scenarios.R`](build_scenarios.R) | `Rscript`-runnable script that downloads upstream reference inputs and uses the `landisutils` API to (re)generate scenario `.txt` files + per-extension config files on top. Prints one absolute scenario directory per line on stdout. |
| [`run_in_docker.sh`](run_in_docker.sh) | Linux Docker driver: runs a single scenario in an ephemeral Docker container. The image is selected per scenario (see [Image selection](#image-selection)). |
| [`install_landis_linux.sh`](install_landis_linux.sh) | Linux native install: extracts `/opt/landis-ii/` from the release docker image into a host dir and exports `LANDIS_CONSOLE_DLL`. |
| [`install_landis_windows.R`](install_landis_windows.R) | Windows native install: downloads the Core WiX MSI and every extension Inno Setup installer (from `extensions-v8-release.yaml`), runs each silently, and exports `LANDIS_CONSOLE_DLL`. |
| [`run_native.R`](run_native.R) | Cross-platform native driver: runs a single scenario via `dotnet Landis.Console.dll` (no container). Used by both `ubuntu-24.04`/`ubuntu-26.04` and `windows-latest` matrix cells of `landis-integration-native.yaml`. |
| [`README.md`](README.md) | This file. |

## Quickstart

Build all scenarios into a temp dir, then run each one through Docker:

```sh
mkdir -p /tmp/landis-scenarios
Rscript inst/integration-tests/build_scenarios.R /tmp/landis-scenarios \
  > /tmp/landis-scenarios.list

# Pre-pull both LANDIS-II images (first run only; cached after).
docker pull ghcr.io/landis-ii-foundation/landis-ii-v8-release:main
docker pull ghcr.io/landis-ii-foundation/landis-ii-v8-uclv2-release:main

while IFS= read -r scen_path; do
  [ -z "$scen_path" ] && continue
  echo "=== $(basename "$scen_path") ==="
  bash inst/integration-tests/run_in_docker.sh "$scen_path" \
    || echo "FAILED: $scen_path"
done < /tmp/landis-scenarios.list
```

A successful LANDIS-II run exits 0; per-run stdout/stderr go to
`<scen_path>/log/`. The console is invoked in parser/run mode, so a
"clean" pass means both scenario syntax and a 50-year simulation
completed without aborting -- but the bar this harness is really designed
to enforce is **parser-level correctness**: any "No extension with the
name ..." or schema-mismatch error fails the run.

## Scenarios produced

Running the script today produces five scenario directories:

| Directory | Image | Extensions | Source |
| --- | --- | --- | --- |
| `biomass_succession` | release | Biomass Succession only | bespoke (Biomass-Succession Core8) |
| `biomass_succession_plus_extras` | release | Biomass Succession + 3 outputs | bespoke (Biomass-Succession Core8) |
| `necn_all_extension__release` | release | 10 (NECN + 9 dist/output) | mirrors upstream `tests/TestNECN_UCLv2_AllExtension/` |
| `necn_all_extension__uclv2` | uclv2 | 10 (same set; all UCL v2-compatible) | mirrors upstream `tests/TestNECN_UCLv2_AllExtension/` |
| `pnet_all_extension__release` | release | 22 (PnET-Succession + 21 dist/output) | mirrors upstream `tests/TestPnET_AllExtension/` |

`pnet_all_extension__uclv2` is **intentionally skipped** because
PnET-Succession is not yet registered in the UCL v2 image (UCL v2
migration is in progress -- see the
[v8-UCL2 extensions YAML](https://github.com/LANDIS-II-Foundation/Tool-Docker-Apptainer/blob/main/extensions-v8-UCL2-release.yaml)).
`build_scenarios.R` logs a `skipping ... (incompatible with this image)`
message and writes no directory.

## Image selection

LANDIS-II v8 is published in two Docker images that register *different*
extension subsets:

| Image | Contents |
| --- | --- |
| `ghcr.io/landis-ii-foundation/landis-ii-v8-release` | Curated v8 release (most extensions; UCL v1 + some UCL v2). Source of truth: [`extensions-v8-release.yaml`](https://github.com/LANDIS-II-Foundation/Tool-Docker-Apptainer/blob/main/extensions-v8-release.yaml). |
| `ghcr.io/landis-ii-foundation/landis-ii-v8-uclv2-release` | Strict subset: only extensions migrated to Universal Cohort Library v2. Source of truth: [`extensions-v8-UCL2-release.yaml`](https://github.com/LANDIS-II-Foundation/Tool-Docker-Apptainer/blob/main/extensions-v8-UCL2-release.yaml). |

`run_in_docker.sh` resolves which image to use in this order:

1. **Per-scenario marker file** — if `<scen_dir>/.landis_image` exists,
   its first non-empty line is used.
2. **`LANDIS_IMAGE` environment variable** — applied to all scenarios in
   the run that lack a marker.
3. **Built-in default** — `landis-ii-v8-release:main`.

`build_scenarios.R` writes a `.landis_image` marker into each
AllExtension scenario directory; the existing `biomass_succession*`
scenarios don't have markers and fall through to the env var / default.

## How filtering works

`build_scenarios.R` builds two **per-image extension allowlists** keyed
by R6 class name (`EXTS_IN_RELEASE_IMAGE`, `EXTS_IN_UCLV2_IMAGE`) by
fetching the upstream YAMLs at script start and translating the
uncommented `repo:` entries through `EXT_REPO_TO_CLASS` (a small mapping
table in the script). For each canonical scenario × image pair,
`filter_extensions_for_image()` drops any `LandisExtension` instance
whose class isn't in the target image's list. If the succession backend
is the one filtered out (e.g. `PnETSuccession` on UCL v2), the entire
(scenario, image) pair is **skipped** -- LANDIS-II can't run without a
succession engine.

The allowlists are not hand-maintained -- they come straight from the
upstream YAMLs at the SHA pinned in `TDA_REF`. Bumping `TDA_REF` is
enough to pick up upstream changes. The only thing that needs human
edits is `EXT_REPO_TO_CLASS` when upstream adds a brand-new extension
that this package also ships an R6 class for; the script warns
(`upstream extension repo(s) have no R6-class mapping (skipped)`) when
that happens, so the missing entries are easy to spot.

## Adding a new scenario

1. Find or build a small upstream input fixture (rasters, CSVs, climate
   tables) you want to mirror -- typically under
   `LANDIS-II-Foundation/Tool-Docker-Apptainer/tests/Test*/inputs/` or an
   extension's own `testings/Core8-*/` directory.
2. Add a new builder function near the existing
   `build_necn_all_extension()` / `build_pnet_all_extension()` pair in
   [`build_scenarios.R`](build_scenarios.R). It should:
   - call `download_repo_subtree(repo, sha, subtree, scen_dir)` (flat
     layout: every file lands in `scen_dir`);
   - construct each extension via its R6 class with parameters mirrored
     from the upstream config (or copied from
     `tests/testthat/test-ext_*.R` when you don't need to match a specific
     fixture);
   - apply `filter_extensions_for_image()`;
   - return `NULL` if the succession backend was filtered out;
   - call `scenario(...)` to write `scenario.txt`;
   - return `scen_dir`.
3. Register it in the driving loop at the bottom of the file.

When parameter values reference ancillary files (rasters, CSVs), use
**absolute paths** (e.g. `file.path(scen_dir, "foo.tif")`). The R6
classes' active bindings call `.relPath(value, self$path)`, which
mistakes bare filenames for CWD-relative.

## CI integration

Two workflows exercise the harness; both reuse the same
`build_scenarios.R` fixtures but differ in how LANDIS-II is invoked
and when they run.

### Linux Docker (every push / PR)

[`.github/workflows/landis-integration.yaml`](../../.github/workflows/landis-integration.yaml)
runs on every push to `main`/`master` and every PR, on
`ubuntu-latest`. It:

1. installs `landisutils` via `R CMD INSTALL`;
2. runs `build_scenarios.R` and captures the scenario list;
3. pulls both LANDIS-II images;
4. invokes `run_in_docker.sh` once per scenario (the marker file picks
   the image);
5. uploads scenario directories as a `landis-scenarios` artifact for
   7 days, regardless of pass/fail -- handy for inspecting parser
   errors offline.

### Native install (opt-in / release / manual)

[`.github/workflows/landis-integration-native.yaml`](../../.github/workflows/landis-integration-native.yaml)
runs a matrix of `ubuntu-24.04`, `ubuntu-26.04`, and `windows-latest`
**only when** one of the following is true:

| Trigger | How to invoke |
| --- | --- |
| Released | Publish a release on GitHub. The workflow runs against the released ref. |
| Manual | "Run workflow" button on the Actions tab (`workflow_dispatch`). |
| Opt-in commit | Include `[ci-windows]` anywhere in the commit message (or PR title / body). |

The native install is skipped on every other push/PR because the
Windows install step is expensive (~5-15 min). The "ubuntu-26 vs
ubuntu-24" pair is there to catch Ubuntu-LTS drift in the docker
runtime environment.

Per-OS install strategy:

| OS | Strategy |
| --- | --- |
| Linux | `install_landis_linux.sh` pulls `ghcr.io/landis-ii-foundation/landis-ii-v8-release:main` and `docker cp`s `/opt/landis-ii/` into `$RUNNER_TEMP/landis-install/`. Reuses the build the Docker workflow trusts -- no rebuild needed. |
| Windows | `install_landis_windows.R` downloads the WiX MSI for the Core console (pinned in `_pins.R`) and runs `msiexec /i ... /quiet /norestart /l*v <log>`. Then iterates every extension repo in `extensions-v8-release.yaml`, fetches the latest `LANDIS-II-V8*-setup.exe`, and runs each silently (`/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-`; all extension installers are Inno Setup 6, confirmed by file inspection). |

Both install paths export `LANDIS_CONSOLE_DLL` to `$GITHUB_ENV` so the
run step can find the console without scanning the install tree
again. The install dirs / installer downloads are cached via
`actions/cache@v4` keyed on `hashFiles('inst/integration-tests/_pins.R')`,
so bumping `_pins.R` invalidates the cache; otherwise the second run
of each matrix cell skips the install download.

After install, the workflow:

1. runs `build_scenarios.R` against `$RUNNER_TEMP/landis-scenarios/`;
2. invokes `run_native.R` per scenario; the helper reads
   `LANDIS_CONSOLE_DLL` and runs `dotnet <dll> <scen.txt>`;
3. uploads scenario directories as
   `landis-scenarios-native-<os>` for 7 days.

The native workflow runs every scenario `build_scenarios.R` emits --
including the `__uclv2` variants. The marker file is only consulted
by `run_in_docker.sh`; native installs ship the full release-image
extension set (a superset of UCL v2), so the same scenario can run on
either side.

A failed scenario surfaces as a `::error::` annotation on the
workflow run; other scenarios continue. **macOS is intentionally not
in the matrix** -- there is no expectation that LANDIS-II runs
natively on macOS, but the package is still useful there for
generating config files (covered by `R-CMD-check.yaml`).

## Caveats / known limitations

- **Parameter values are not byte-for-byte fidelity to upstream.** The
  scenario builders aim for parser-compatible output; many constructor
  arguments are borrowed from this package's existing
  `tests/testthat/test-ext_*.R` Core8 fixtures rather than transcribed
  verbatim from `Tool-Docker-Apptainer`. The scenarios are designed to
  exercise the package's writers against the LANDIS-II parser, not to be
  ecologically meaningful runs.
- **`LandisData` string drift.** A few extensions in this package emit a
  `LandisData` name that differs slightly from what the upstream parser
  expects (e.g. `"Land Use"` here vs `"Land Use Change"` upstream). When
  the integration test fails with `No extension with the name ...`, the
  fix is in the relevant `R/ext_*.R`, not here.
- **Tarball pin.** `build_scenarios.R` pins
  `Tool-Docker-Apptainer` to a specific commit SHA (constant
  `TDA_REF` near the top). Bump it when you want to pull in upstream
  fixture changes.
- **Network access at build time.** The script downloads from
  `raw.githubusercontent.com` and `codeload.github.com`. Behind a
  restrictive proxy, set `R`'s `download.file.method` and `HTTPS_PROXY`
  before running.
