## Incremental streaming of completed LANDIS-II output maps off local scratch.
##
## A replicate is run on node-local scratch and only moved to durable storage once it finishes
## (landis_archive_rep()), so peak scratch is (concurrent replicates) x (WHOLE replicate). At
## production length that does not scale: a 1200-year run writing several rasters per timestep is
## tens of GB, and a node's replicate concurrency is sized against RAM, not disk -- so a large enough
## run exhausts scratch hours in, after most of the compute has been paid for.
##
## Streaming moves each timestep's output maps to durable storage DURING the run, so scratch only
## ever holds the working set: whatever has been written since the last sync, plus a small lag.
##
## Nothing is discarded -- files are MOVED, not pruned. Outputs that no downstream target reads today
## are still archived, because re-running 1200 years to recover them is far more expensive than
## storing them.
##
## Crucially, streaming writes into the SAME `<final_dir>.partial` staging directory that
## landis_archive_rep() already uses, so the atomic-rename publish is preserved: `final_dir` still
## only ever appears complete, and a partially-streamed replicate is never mistaken for a finished
## one by a downstream skip-check.

## Directories LANDIS-II extensions write their per-timestep output maps into.
##
## Scoping by DIRECTORY rather than by map name is what makes this work for every extension instead
## of just the ones one project happens to use. Output maps follow a universal
## `<dir>/<name>-{timestep}.<ext>` convention (see `MapNames()`), so enumerating names would mean
## chasing each new extension; enumerating the handful of output directories covers Dynamic
## Fire/Fuels, ForCS, NECN, PnET, Biomass/Century, harvest, BDA, EDA, wind, hurricane, root rot and
## the Output-* family at once.
##
## It is also the safer shape. A bare `-{timestep}.tif` pattern would match `landuse-{timestep}.tif`,
## which Land Use Plus READS as input, at the scenario root -- moving it would break the run. Only
## files inside a known output directory are eligible, so root-level inputs are structurally
## excluded rather than excluded by memory.
.default_stream_dirs <- function() {
  c(
    "fire", # Dynamic Fire, Original Fire, Social Climate Fire
    "wind",
    "linearwind",
    "hurricane",
    "harvest", # Biomass Harvest, Magic Harvest
    "bda", # (Climate) Biological Disturbance Agents
    "eda", # Epidemiological Disturbance Agents
    "rootrot",
    "NECN",
    "ForCS",
    "century",
    "output", # Output-* family (biomass, LAI, senescence, soil water, habitat, ...)
    "outputs", # newer Output-* extensions use the plural form
    "DFFS-output"
  )
}

## Map basenames that are NOT safe to move, even inside an output directory.
##
## These look like ordinary output maps but may be simulation STATE that the extension reads back on
## a later timestep. Nothing here has verified against the extension sources that they are
## write-only, and moving a file the simulation later reads corrupts the run SILENTLY -- no error,
## just wrong results. They are a small share of the volume, so excluding them costs little.
.default_stream_exclude <- function() {
  c(
    "^TimeOfLast", # Dynamic Fire: time since last fire
    "^TimeSince",
    "^TOLD" # root rot: time of last disturbance
  )
}

## Regexes matching output maps that are safe to move mid-run: inside a known output directory, with
## a timestep-suffixed name, and not on the exclusion list.
.default_stream_patterns <- function(dirs = .default_stream_dirs()) {
  sprintf("(^|/)%s/.*[^0-9]([0-9]+)\\.(tif|img)$", gsub("([.\\-])", "\\\\\\1", dirs))
}

## Last completed timestep according to the run log, or NA when it cannot be read yet.
.landis_current_timestep <- function(log_path) {
  if (!fs::file_exists(log_path)) {
    return(NA_integer_)
  }
  ln <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) character(0))
  hits <- grep("Current time:\\s*[0-9]+", ln, value = TRUE)
  if (!length(hits)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(sub(".*Current time:\\s*([0-9]+).*", "\\1", hits[length(hits)])))
}

## Timestep embedded in an output map's filename, or NA when there is none. A file without a
## parseable timestep is never streamed: the allow-list is positive, so anything unrecognised stays
## on scratch and is handled by the final archive.
.stream_file_timestep <- function(paths) {
  suppressWarnings(as.integer(sub(".*[^0-9]([0-9]+)\\.(tif|img)$", "\\1", basename(paths))))
}

## Move every output map for timestep <= (current - lag) into `dest`, preserving paths relative to
## `run_dir`, and delete the local copy ONLY after rsync reports success.
##
## The lag is what makes "written" mean "closed": LANDIS-II emits several rasters per timestep, so a
## file for step t can still be open when the log already reports `Current time: t`. Two timesteps of
## slack costs almost nothing and removes the race entirely.
##
## Returns, invisibly, the number of files moved. Never signals: a storage hiccup must not fail a
## simulation that is otherwise healthy, so the caller simply retries at the next interval.
.stream_completed_outputs <- function(
  run_dir,
  dest,
  lag_steps = 2L,
  patterns = .default_stream_patterns(),
  exclude = .default_stream_exclude(),
  log_path = fs::path(run_dir, "Landis-log.txt")
) {
  current <- .landis_current_timestep(log_path)
  if (is.na(current)) {
    return(invisible(0L))
  }
  safe_t <- current - as.integer(lag_steps)
  if (safe_t < 0L) {
    return(invisible(0L))
  }

  all_files <- tryCatch(
    fs::dir_ls(run_dir, recurse = TRUE, type = "file", fail = FALSE),
    error = function(e) character(0)
  )
  if (!length(all_files)) {
    return(invisible(0L))
  }
  rel <- as.character(fs::path_rel(all_files, run_dir))
  keep <- Reduce(`|`, lapply(patterns, function(p) grepl(p, rel)), init = rep(FALSE, length(rel)))
  if (length(exclude)) {
    bn <- basename(rel)
    drop <- Reduce(`|`, lapply(exclude, function(p) grepl(p, bn)), init = rep(FALSE, length(bn)))
    keep <- keep & !drop
  }
  if (!any(keep)) {
    return(invisible(0L))
  }
  ts <- .stream_file_timestep(all_files[keep])
  ready <- !is.na(ts) & ts <= safe_t
  if (!any(ready)) {
    return(invisible(0L))
  }
  src_rel <- rel[keep][ready]
  src_abs <- all_files[keep][ready]

  ## Creating the destination can itself fail (share unmounted, permissions), and this function
  ## promises never to signal -- a storage problem must cost a retry, not the simulation.
  if (inherits(tryCatch(fs::dir_create(dest), error = function(e) e), "error")) {
    return(invisible(0L))
  }
  ## --files-from keeps the relative layout without one rsync per file, which matters when 50
  ## replicates sync concurrently over a network share.
  list_file <- fs::file_temp(ext = "txt")
  on.exit(unlink(list_file), add = TRUE)
  writeLines(src_rel, list_file)

  res <- tryCatch(
    processx::run(
      "rsync",
      c("-a", "--files-from", list_file, paste0(run_dir, "/"), paste0(dest, "/")),
      error_on_status = FALSE,
      echo = FALSE
    ),
    error = function(e) list(status = 127L)
  )
  if (!identical(as.integer(res$status), 0L)) {
    return(invisible(0L)) ## leave everything in place; retry next interval
  }
  ## Verified copy exists -> reclaim the scratch copy.
  unlink(src_abs)
  invisible(length(src_abs))
}

## Next sync time, jittered.
##
## Without jitter every replicate on a node syncs on the same cadence, and since they are launched
## together they stay in phase for the whole run -- turning a steady trickle into a periodic burst of
## concurrent writes to one network share. Jitter spreads them, in the same spirit as the existing
## container-startup stagger.
.next_stream_at <- function(now, every_sec, jitter_frac = 0.25) {
  f <- max(0, min(1, jitter_frac))
  now + every_sec * stats::runif(1L, 1 - f, 1 + f)
}
