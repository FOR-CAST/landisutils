#!/usr/bin/env bash
##
## Run a LANDIS-II scenario in an ephemeral docker container.
##
## Usage:
##   run_in_docker.sh <scenario_dir> [<scenario_file>]
##
## - <scenario_dir>  Host directory containing the scenario file and all of
##                   its referenced inputs.
## - <scenario_file> Filename of the scenario file inside <scenario_dir>.
##                   Defaults to "$(basename <scenario_dir>).txt".
##
## Image resolution order (first match wins):
##   1. <scenario_dir>/.landis_image  -- per-scenario marker file (one image
##                                       reference, on the first non-empty line)
##   2. LANDIS_IMAGE                  -- environment variable
##   3. Built-in default              -- ghcr.io/.../landis-ii-v8-release:main
##
## Other environment overrides:
##   LANDIS_CONSOLE_DLL  Path inside the image to Landis.Console.dll.
##                       Default: /opt/landis-ii/Core-Model-v8-LINUX/build/Release/Landis.Console.dll
##
## The image's CMD is /bin/bash, so we override the entrypoint to `dotnet`
## and pass the console DLL + scenario file as plain arguments.

set -euo pipefail

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
  echo "usage: $0 <scenario_dir> [<scenario_file>]" >&2
  exit 64
fi

scen_dir=$(realpath -- "$1")
scen_file=${2:-"$(basename -- "$scen_dir").txt"}

if [ -f "${scen_dir}/.landis_image" ]; then
  image=$(grep -m1 -v '^[[:space:]]*$' "${scen_dir}/.landis_image" | tr -d '[:space:]')
else
  image=${LANDIS_IMAGE:-ghcr.io/landis-ii-foundation/landis-ii-v8-release:main}
fi
console=${LANDIS_CONSOLE_DLL:-/opt/landis-ii/Core-Model-v8-LINUX/build/Release/Landis.Console.dll}

if [ ! -f "${scen_dir}/${scen_file}" ]; then
  echo "error: ${scen_dir}/${scen_file} does not exist" >&2
  exit 66
fi

exec docker run --rm \
  --entrypoint dotnet \
  --user "$(id -u):$(id -g)" \
  -v "${scen_dir}:/sim" \
  -w /sim \
  "${image}" \
  "${console}" \
  "${scen_file}"
