## Warm Docker pool for LANDIS-II calibration runs --------------------------------------
##
## Per-trial `docker run --rm` startup adds ~1-3 s of container creation + bind-mount setup
## on top of LANDIS-II's own dotnet startup. For a DEoptim calibration with hundreds-to-
## thousands of trials, that's hours of pure overhead. The pool starts N detached
## containers at calibration entry; each trial runs `docker exec <container> dotnet ...`
## against a per-trial scratch sub-directory inside a shared bind-mount.
##
## Isolation contract: each trial uses a fresh sub-directory (created and deleted by the
## caller, typically `sim_landis()`). The container itself is re-used across trials, but:
##   - each `docker exec` is a fresh dotnet process (no in-process state carries over);
##   - `--env HOME=/tmp` and `--env DOTNET_BUNDLE_EXTRACT_BASE_DIR=...` keep dotnet from
##     writing to long-lived per-user caches;
##   - per-trial output files land in the trial's own rep dir, deleted after parsing.
##
## See also: [landis_run_docker()] for the one-off (non-pool) Docker execution path.

#' Start a warm Docker pool for LANDIS-II calibration
#'
#' Spawns `n` detached containers from `image`, each bind-mounting `scratch_root`
#' to `/scratch` inside. Each container runs a `sleep`-based busy loop that
#' responds to `docker stop` by exiting cleanly. The intended workflow is to
#' dispatch many `landis_pool_exec()` calls to the pool, then call
#' `landis_pool_stop()` to tear it down -- typically via `on.exit()` in the
#' calling driver.
#'
#' @param n Integer. Number of pool containers to start. Typically the number
#'   of parallel DEoptim workers.
#' @param image Character or NULL. Docker image reference. NULL =
#'   `getOption("landisutils.docker.image")`.
#' @param scratch_root Character. Host directory bind-mounted to `/scratch`
#'   inside every container. Must be a stable absolute path. All per-trial
#'   directories used by [landis_pool_exec()] must be SUBDIRECTORIES of this.
#' @param cpu_limit Numeric or NULL. Per-container `--cpus` cap. Default 4.
#' @param mem_limit Character or NULL. Per-container `--memory` cap. Default `"8g"`.
#' @param pull Logical. When TRUE, `docker pull` the image before starting.
#' @param name_prefix Character or NULL. Optional prefix for the container name;
#'   the rest is auto-generated to avoid collisions across concurrent pools.
#'
#' @returns A list with `names` (container names), `image`, `scratch_root`, `n`,
#'   `started_at`, and `digest` (image RepoDigest if available).
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_pool_exec()], [landis_pool_stop()], [landis_run_docker()]
#'
#' @export
landis_pool_start <- function(
  n,
  image = NULL,
  scratch_root,
  cpu_limit = 4,
  mem_limit = "8g",
  pull = FALSE,
  name_prefix = NULL
) {
  stopifnot(
    is.numeric(n),
    n >= 1L,
    is.character(scratch_root),
    length(scratch_root) == 1L,
    fs::dir_exists(scratch_root)
  )
  image <- image %||% getOption("landisutils.docker.image")
  scratch_root <- fs::path_real(scratch_root)
  n <- as.integer(n)

  if (isTRUE(pull)) {
    message(glue::glue("landis_pool_start: pulling {image}"))
    pull_res <- processx::run("docker", c("pull", image), error_on_status = FALSE, echo = FALSE)
    if (pull_res$status != 0L) {
      warning(
        sprintf(
          "docker pull %s failed (rc=%d); continuing with local image.",
          image,
          pull_res$status
        ),
        call. = FALSE
      )
    }
  }

  ## Capture image digest for provenance (matches landis_run_docker() behaviour).
  digest_line <- tryCatch(
    {
      res <- processx::run(
        "docker",
        c(
          "image",
          "inspect",
          image,
          "--format",
          "{{if .RepoDigests}}{{index .RepoDigests 0}}{{else}}{{.Id}}{{end}}"
        ),
        error_on_status = FALSE,
        echo = FALSE
      )
      out <- trimws(res$stdout)
      if (nzchar(out)) out else NA_character_
    },
    error = function(e) NA_character_
  )

  ## --user uid:gid (POSIX only). Read once via processx so we don't pay
  ## a shell-quoting tax inside loops.
  user_args <- if (.Platform$OS.type != "windows") {
    uid <- trimws(processx::run("id", "-u")$stdout)
    gid <- trimws(processx::run("id", "-g")$stdout)
    c("--user", paste0(uid, ":", gid))
  } else {
    character(0)
  }

  cpu_args <- if (is.null(cpu_limit) || is.infinite(cpu_limit)) {
    character(0)
  } else {
    c("--cpus", as.character(cpu_limit))
  }
  mem_args <- if (is.null(mem_limit) || identical(mem_limit, Inf)) {
    character(0)
  } else {
    c("--memory", as.character(mem_limit))
  }

  ## Unique pool ID to avoid name collisions across concurrent calibrations.
  pool_id <- paste0(
    name_prefix %||% "landis-pool",
    "-",
    Sys.getpid(),
    "-",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "-",
    sprintf("%04d", sample.int(9999L, 1L))
  )

  ## Start containers. The `trap` lets `docker stop` exit cleanly; `--rm` cleans up the
  ## container record on exit; the busy `sleep` keeps the container alive between exec calls.
  container_names <- vapply(
    seq_len(n),
    function(i) {
      name <- sprintf("%s-%02d", pool_id, i)
      .landis_pool_start_one(
        name = name,
        image = image,
        scratch_root = scratch_root,
        user_args = user_args,
        cpu_args = cpu_args,
        mem_args = mem_args
      )
    },
    character(1)
  )

  message(glue::glue("landis_pool_start: {n} container(s) started ({pool_id}, image {image})"))

  structure(
    list(
      names = container_names,
      image = image,
      digest = digest_line,
      scratch_root = scratch_root,
      n = n,
      started_at = Sys.time(),
      pool_id = pool_id,
      ## Retain the args used to start the pool so landis_pool_restart_one()
      ## can recreate a container with identical config.
      user_args = user_args,
      cpu_args = cpu_args,
      mem_args = mem_args
    ),
    class = "landis_pool"
  )
}

## Internal: start a single warm container with the given name + args.
## Returns the container name on success; stops with diagnostics on failure.
.landis_pool_start_one <- function(name, image, scratch_root, user_args, cpu_args, mem_args) {
  args <- c(
    "run",
    "-d",
    "--rm",
    "--name",
    name,
    ## /bin/sh is universally available across Linux base images (busybox /
    ## alpine / debian / Microsoft dotnet runtime images); /bin/bash is not.
    "--entrypoint",
    "/bin/sh",
    user_args,
    cpu_args,
    mem_args,
    "-v",
    paste0(scratch_root, ":/scratch"),
    "-w",
    "/scratch",
    image,
    "-c",
    "trap 'exit 0' TERM; while true; do sleep 60; done"
  )
  ## Use processx::run rather than system2 -- the container command contains
  ## a semicolon which `system2` would route through /bin/sh -c, splitting it
  ## as two shell commands and only the first would land inside the container.
  res <- processx::run("docker", args, error_on_status = FALSE, echo = FALSE)
  if (res$status != 0L) {
    stop(
      "failed to start pool container ",
      name,
      " (status ",
      res$status,
      ")\n",
      "stderr: ",
      substr(res$stderr, 1L, 500L),
      call. = FALSE
    )
  }
  name
}

#' Execute a command in one of the warm-pool containers
#'
#' Runs `docker exec --workdir <workdir> <container> <command> <args>` in the
#' container at index `idx` of `pool`. Captures stdout/stderr to disk if requested.
#' Each invocation is a fresh dotnet process in the container -- no in-process
#' state from prior calls carries over -- and the per-trial working directory
#' isolates output files.
#'
#' For LANDIS-II calibration trials specifically, see [sim_landis()] which
#' wraps this with the trial-directory copy + dynamic-fire.txt patch logic.
#'
#' @param pool A `landis_pool` object from [landis_pool_start()].
#' @param idx Integer. 1-based container index within the pool.
#' @param workdir Character. Working directory INSIDE the container (i.e.,
#'   under `/scratch/`). Must correspond to a host path under `pool$scratch_root`.
#' @param command Character. Executable to run in the container (e.g., "dotnet").
#' @param args Character vector. Arguments passed to `command`.
#' @param timeout_sec Numeric or NULL. Maximum wait time. NULL = no timeout.
#' @param stdout_log,stderr_log Character or NULL. Paths to write captured output.
#' @param extra_env Named character. Extra environment variables to set via
#'   `--env`. Default sets `HOME=/tmp` and `DOTNET_BUNDLE_EXTRACT_BASE_DIR` to
#'   a unique-per-call tempdir, isolating dotnet's per-user caches between
#'   trials.
#' @param retries Integer >= 0. If the exec command fails with a non-zero exit
#'   status, restart the container via [landis_pool_restart_one()] and try
#'   again, up to `retries` extra attempts. Default 0 = fail-fast (matches
#'   prior behaviour). Useful when long calibrations occasionally see
#'   container crashes (OOM, daemon hiccup) without wanting to abort.
#'
#' @returns A list with `status` (integer exit code), `elapsed_sec` (numeric),
#'   `container` (the container name), and `attempts` (1 + number of retries
#'   actually consumed).
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_pool_start()], [landis_pool_stop()], [landis_pool_restart_one()]
#'
#' @export
landis_pool_exec <- function(
  pool,
  idx,
  workdir,
  command,
  args = character(),
  timeout_sec = NULL,
  stdout_log = NULL,
  stderr_log = NULL,
  extra_env = NULL,
  retries = 0L
) {
  stopifnot(
    inherits(pool, "landis_pool"),
    is.numeric(idx),
    idx >= 1L,
    idx <= pool$n,
    is.character(workdir),
    length(workdir) == 1L,
    is.character(command),
    length(command) == 1L,
    is.numeric(retries),
    retries >= 0L
  )

  ## Default isolation env: redirect $HOME and dotnet's bundle-extract dir away from any
  ## long-lived cache locations so successive trials don't inherit state through them.
  call_id <- sprintf("trial-%d-%s", Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS3"))
  default_env <- c(
    HOME = "/tmp",
    DOTNET_BUNDLE_EXTRACT_BASE_DIR = paste0("/tmp/dotnet_bundle_", call_id)
  )
  if (!is.null(extra_env)) {
    default_env[names(extra_env)] <- extra_env
  }
  env_args <- as.vector(rbind("--env", paste0(names(default_env), "=", default_env)))

  attempts <- 0L
  max_attempts <- 1L + as.integer(retries)
  last_status <- NA_integer_
  last_stderr <- ""
  while (attempts < max_attempts) {
    attempts <- attempts + 1L
    container <- pool$names[as.integer(idx)]
    exec_args <- c("exec", "--workdir", workdir, env_args, container, command, args)

    t_start <- proc.time()
    res <- processx::run(
      "docker",
      exec_args,
      timeout = if (is.null(timeout_sec)) Inf else as.numeric(timeout_sec),
      error_on_status = FALSE,
      echo = FALSE
    )
    elapsed_sec <- (proc.time() - t_start)[["elapsed"]]

    if (!is.null(stdout_log)) {
      fs::dir_create(dirname(stdout_log))
      writeLines(res$stdout, stdout_log)
    }
    if (!is.null(stderr_log)) {
      fs::dir_create(dirname(stderr_log))
      writeLines(res$stderr, stderr_log)
    }

    if (res$status == 0L) {
      return(invisible(list(
        status = res$status,
        elapsed_sec = elapsed_sec,
        container = container,
        attempts = attempts
      )))
    }
    last_status <- res$status
    last_stderr <- res$stderr

    if (attempts < max_attempts) {
      message(sprintf(
        "landis_pool_exec: container %s failed (status %d, %.1fs); restarting and retrying (%d / %d).",
        container,
        res$status,
        elapsed_sec,
        attempts,
        max_attempts - 1L
      ))
      landis_pool_restart_one(pool, idx)
    }
  }

  stop(
    "landis_pool_exec: command failed in container ",
    pool$names[as.integer(idx)],
    " after ",
    attempts,
    " attempt(s) (last status ",
    last_status,
    ", elapsed ",
    round(elapsed_sec, 1),
    "s)\n",
    "stderr (first 500 chars): ",
    substr(last_stderr, 1L, 500L),
    call. = FALSE
  )
}

#' Restart a single container in a warm Docker pool
#'
#' Stops + removes the container at index `idx` if it exists, then starts a fresh
#' replacement with identical config (image, scratch_root bind-mount, user,
#' cpu_limit, mem_limit) using a new auto-generated container name. The pool
#' object's `$names[idx]` is updated to point at the new container.
#'
#' Intended for use by [landis_pool_exec()]'s `retries` mechanism, but also
#' safe to call directly when a calibration driver detects a container is
#' wedged or has been OOM-killed.
#'
#' @param pool A `landis_pool` object from [landis_pool_start()].
#' @param idx Integer. 1-based index of the container to replace.
#'
#' @returns The pool (invisibly), with `$names[idx]` updated to the new
#'   container name.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_pool_start()], [landis_pool_exec()], [landis_pool_stop()]
#'
#' @export
landis_pool_restart_one <- function(pool, idx) {
  stopifnot(inherits(pool, "landis_pool"), is.numeric(idx), idx >= 1L, idx <= pool$n)
  old_name <- pool$names[as.integer(idx)]

  ## Stop + force-remove the old container (idempotent).
  tryCatch(
    processx::run(
      "docker",
      c("stop", "--time", "10", old_name),
      error_on_status = FALSE,
      echo = FALSE
    ),
    error = function(e) NULL
  )
  tryCatch(
    processx::run("docker", c("rm", "-f", old_name), error_on_status = FALSE, echo = FALSE),
    error = function(e) NULL
  )

  ## Start a replacement with a fresh name based on the pool id + index +
  ## a short random suffix (so multiple restarts don't collide).
  new_name <- sprintf(
    "%s-%02d-r%s",
    pool$pool_id,
    as.integer(idx),
    sprintf("%04d", sample.int(9999L, 1L))
  )
  pool$names[as.integer(idx)] <- .landis_pool_start_one(
    name = new_name,
    image = pool$image,
    scratch_root = pool$scratch_root,
    user_args = pool$user_args,
    cpu_args = pool$cpu_args,
    mem_args = pool$mem_args
  )

  ## The caller holds the pool by reference (it's a list) but R is copy-on-modify.
  ## Return the updated pool so callers (incl. landis_pool_exec's retry loop)
  ## see the new name. landis_pool_exec mutates its local `pool` via direct
  ## reassignment after this call -- actually, R won't propagate the rename
  ## back to the caller's pool unless we use <<- or explicit return. We do
  ## both: return the pool (caller can choose to update), and additionally
  ## use eval.parent() to update the caller's `pool` object in-place when
  ## called from landis_pool_exec.
  ##
  ## Implementation note: instead of relying on lexical-frame mutation magic,
  ## landis_pool_exec re-reads `pool$names[idx]` at the start of each attempt;
  ## here we use parent.frame() to update the caller's `pool` if it has one.
  parent_env <- parent.frame()
  if (exists("pool", envir = parent_env, inherits = FALSE)) {
    parent_pool <- get("pool", envir = parent_env)
    if (inherits(parent_pool, "landis_pool")) {
      parent_pool$names[as.integer(idx)] <- new_name
      assign("pool", parent_pool, envir = parent_env)
    }
  }

  invisible(pool)
}

#' Stop and remove all containers in a warm Docker pool
#'
#' Sends `docker stop` (graceful SIGTERM, the container's trap exits cleanly)
#' then `docker rm -f` (idempotent belt-and-suspenders) for every container in
#' the pool. Safe to call multiple times.
#'
#' Typical use is via `on.exit(landis_pool_stop(pool), add = TRUE)` so the pool
#' is cleaned up even when the calling DEoptim driver errors out.
#'
#' @param pool A `landis_pool` object from [landis_pool_start()].
#' @param timeout_sec Numeric. Graceful-stop timeout passed to `docker stop`.
#'   Default 10.
#'
#' @returns The pool (invisibly), with `stopped_at` added.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_pool_start()], [landis_pool_exec()]
#'
#' @export
landis_pool_stop <- function(pool, timeout_sec = 10) {
  stopifnot(inherits(pool, "landis_pool"))
  ## `docker stop` and `docker rm` both accept many container names per invocation
  ## and parallelise internally on the daemon side. The prior per-container loop
  ## was O(n * timeout_sec) on graceful stop -- 15+ minutes for a 90-container
  ## calibration pool -- because each docker-stop call serialises on its own
  ## SIGTERM/SIGKILL deadline. Passing all names in one shot lets the daemon
  ## run the stops concurrently, so the wall time becomes ~timeout_sec total.
  if (length(pool$names) > 0L) {
    tryCatch(
      processx::run(
        "docker",
        c("stop", "--time", as.character(timeout_sec), pool$names),
        error_on_status = FALSE,
        echo = FALSE
      ),
      error = function(e) NULL
    )
    ## --rm at run time should auto-clean, but force-remove in case the container
    ## already exited non-cleanly.
    tryCatch(
      processx::run("docker", c("rm", "-f", pool$names), error_on_status = FALSE, echo = FALSE),
      error = function(e) NULL
    )
  }
  pool$stopped_at <- Sys.time()
  invisible(pool)
}
