#!/usr/bin/env bash
##
## Set up a *native* LANDIS-II install on a Linux host by extracting the
## already-built /opt/landis-ii tree from the official docker image. The
## image is the source of truth for the Linux build pipeline, so this
## avoids reproducing the build steps (and tracking their Ubuntu-version
## drift) on the runner.
##
## After this script runs, `Landis.Console.dll` is at
## "$LANDIS_INSTALL_DIR/Core-Model-v8-LINUX/build/Release/Landis.Console.dll"
## and the path is exported to $GITHUB_ENV as LANDIS_CONSOLE_DLL so the
## scenario-run step can find it without a second filesystem scan.
##
## Usage:
##   bash install_landis_linux.sh [--image <ref>] [--install-dir <path>]
##
## Defaults:
##   --image       $LANDIS_IMAGE        (or the release image from _pins.R)
##   --install-dir $LANDIS_INSTALL_DIR  (or $RUNNER_TEMP/landis-install,
##                                       or /tmp/landis-install)

set -euo pipefail

usage() {
  cat >&2 <<EOF
usage: $0 [--image <ref>] [--install-dir <path>]
EOF
  exit 64
}

image=""
install_dir=""
while [ $# -gt 0 ]; do
  case "$1" in
    --image)        image=$2; shift 2 ;;
    --install-dir)  install_dir=$2; shift 2 ;;
    -h|--help)      usage ;;
    *)              echo "unknown arg: $1" >&2; usage ;;
  esac
done

## Default image: read from _pins.R via R so the constant lives in one place.
if [ -z "$image" ]; then
  image=${LANDIS_IMAGE:-}
fi
if [ -z "$image" ]; then
  script_dir=$(cd "$(dirname "$0")" && pwd)
  image=$(Rscript -e "source('$script_dir/_pins.R'); cat(LANDIS_IMAGE_RELEASE)")
fi
if [ -z "$image" ]; then
  echo "error: could not resolve LANDIS image (set LANDIS_IMAGE or fix _pins.R)" >&2
  exit 70
fi

if [ -z "$install_dir" ]; then
  install_dir=${LANDIS_INSTALL_DIR:-${RUNNER_TEMP:-/tmp}/landis-install}
fi
mkdir -p "$install_dir"
install_dir=$(cd "$install_dir" && pwd)

echo "image:        $image"
echo "install_dir:  $install_dir"

echo "::group::pull image"
docker pull "$image"
echo "::endgroup::"

echo "::group::extract /opt/landis-ii"
container=$(docker create "$image")
trap 'docker rm -f "$container" >/dev/null 2>&1 || true' EXIT
docker cp "$container:/opt/landis-ii/." "$install_dir/"
docker rm -f "$container" >/dev/null
trap - EXIT
echo "::endgroup::"

console_dll=$(find "$install_dir" -type f -iname 'Landis.Console.dll' | head -n1 || true)
if [ -z "$console_dll" ]; then
  echo "error: Landis.Console.dll not found under $install_dir" >&2
  ls -la "$install_dir" >&2
  exit 71
fi

echo "Landis.Console.dll: $console_dll"

if [ -n "${GITHUB_ENV:-}" ]; then
  printf 'LANDIS_CONSOLE_DLL=%s\n' "$console_dll" >> "$GITHUB_ENV"
  echo "exported LANDIS_CONSOLE_DLL to GITHUB_ENV"
fi
