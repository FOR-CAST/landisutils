## Multi-node execution for the Dynamic Fire calibration.
##
## The single-node path builds a FORK cluster whose children inherit one shared `landis_pool`, and
## gives each child a 1-based index into `pool$names` via the LANDIS_POOL_CONTAINER_IDX env var.
## FORK is local-only, so that design caps the calibration at one host -- and per-node throughput
## saturates well before the CPUs do (the workload is memory-bandwidth-bound: on a dual-socket EPYC
## 7702, growing the pool 22 -> 90 containers bought only ~1.25x throughput while per-rep time grew
## ~5x). Spreading the same worker count over more hosts is therefore worth more than the host count
## alone suggests, because each host returns to its unsaturated regime.
##
## The multi-node path swaps FORK for PSOCK and gives every worker its OWN one-container pool on its
## OWN host. That is what makes the container index a non-issue: `landis_pool_exec()` resolves
## `pool$names[idx]` and shells out to the LOCAL docker daemon, so a worker only ever needs a pool
## describing containers on the machine it is running on. No cross-host index mapping exists, and
## `landis_pool_exec()` is untouched.

## Worker-local state. Populated on PSOCK workers by .worker_pool_start(); empty in the coordinator
## and on FORK children, which is precisely what makes .resolve_pool() fall back to the shared-pool
## behaviour and leaves the single-node path byte-for-byte unchanged.
.worker_pool_env <- new.env(parent = emptyenv())

## Start this worker's own single-container pool. Runs ON the worker. `name_prefix` is suffixed with
## the worker's pid inside landis_pool_start()'s pool_id, so two workers on the same host cannot
## collide on a container name.
.worker_pool_start <- function(image, scratch_root, cpu_limit, mem_limit, pull, name_prefix) {
  ## The coordinator only dir_create()s scratch_root on ITS OWN filesystem. scratch_root is a
  ## node-local path (Docker cannot bind-mount the root-squashed NFS project share), so every worker
  ## has to materialise its own copy before the pool can bind-mount it.
  fs::dir_create(scratch_root)
  p <- landis_pool_start(
    n = 1L,
    image = image,
    scratch_root = scratch_root,
    cpu_limit = cpu_limit,
    mem_limit = mem_limit,
    pull = pull,
    name_prefix = name_prefix
  )
  assign(".worker_pool", p, envir = .worker_pool_env)
  invisible(p$names)
}

## Tear down this worker's pool. Runs ON the worker. Safe to call when nothing was started.
.worker_pool_stop <- function() {
  p <- get0(".worker_pool", envir = .worker_pool_env, ifnotfound = NULL)
  if (!is.null(p)) {
    tryCatch(landis_pool_stop(p), error = function(e) invisible(NULL))
    rm(".worker_pool", envir = .worker_pool_env)
  }
  invisible(NULL)
}

## Report whether THIS worker's container is actually running. Runs ON the worker.
##
## Pool containers are started with `--rm`, so one that dies is REMOVED rather than left behind in
## "exited" -- `docker ps -a` shows nothing at all. That makes a dead container invisible to every
## after-the-fact check, while the worker's `.worker_pool` object happily goes on naming it. The only
## honest test is to ask the daemon whether the name still resolves to a RUNNING container.
.worker_pool_probe <- function() {
  host <- as.character(Sys.info()[["nodename"]])
  p <- get0(".worker_pool", envir = .worker_pool_env, ifnotfound = NULL)
  if (is.null(p) || !length(p$names)) {
    return(list(host = host, container = NA_character_, running = FALSE))
  }
  nm <- p$names[[1L]]
  ## shQuote the format string for the same reason .verify_node_images() does: system2() does not
  ## quote, so an unquoted "{{.State.Running}}" would split into shell words.
  st <- suppressWarnings(system2(
    "docker",
    c("inspect", "--format", shQuote("{{.State.Running}}"), shQuote(nm)),
    stdout = TRUE,
    stderr = FALSE
  ))
  list(
    host = host,
    container = nm,
    running = length(st) > 0L && identical(trimws(st[[1L]]), "true")
  )
}

## Replace THIS worker's container in place. Runs ON the worker. Returns TRUE if the restart call
## itself succeeded; the caller re-probes rather than trusting this.
.worker_pool_heal <- function() {
  p <- get0(".worker_pool", envir = .worker_pool_env, ifnotfound = NULL)
  if (is.null(p)) {
    return(FALSE)
  }
  tryCatch(
    {
      landis_pool_restart_one(p, 1L)
      TRUE
    },
    error = function(e) FALSE
  )
}

## Resolve which pool + index this evaluation should run in. A worker-local pool always wins; the
## shared pool + env-var index is the single-node fallback.
.resolve_pool <- function(pool) {
  wp <- get0(".worker_pool", envir = .worker_pool_env, ifnotfound = NULL)
  if (!is.null(wp)) {
    return(list(pool = wp, idx = 1L))
  }
  if (!is.null(pool)) {
    return(list(pool = pool, idx = as.integer(Sys.getenv("LANDIS_POOL_CONTAINER_IDX", "1"))))
  }
  list(pool = NULL, idx = NULL)
}

## Normalise cfg$nodes into a named integer vector of requested workers per host.
.parse_nodes <- function(nodes) {
  if (is.null(nodes) || !length(nodes)) {
    return(NULL)
  }
  if (is.null(names(nodes)) || any(!nzchar(names(nodes)))) {
    stop("`cfg$nodes` must be a NAMED vector, e.g. c(host1 = 30, host2 = 30).", call. = FALSE)
  }
  n <- stats::setNames(as.integer(nodes), names(nodes))
  if (any(is.na(n)) || any(n < 1L)) {
    stop("`cfg$nodes` values must all be >= 1.", call. = FALSE)
  }
  n
}

## The renv project root to activate on workers, or "" when renv is not in use. Requires the project
## to live at the SAME path on every host, which is how these nodes are kept in sync.
.renv_project <- function() {
  p <- Sys.getenv("RENV_PROJECT", "")
  if (nzchar(p) && file.exists(file.path(p, "renv", "activate.R"))) p else ""
}

## Rewrite any host that IS this machine to "localhost".
##
## The coordinator normally runs ON one of the fleet hosts (the crew worker that executes the
## calibration target is placed on the first configured host), so the fleet virtually always includes
## the local machine. Naming it explicitly makes parallelly open an SSH connection from the host to
## ITSELF, and a machine's own name need not be reachable over SSH from itself: here it resolves to
## 127.0.1.1, where sshd is not accepting, so the connection is refused and cluster setup stalls.
## "localhost" makes parallelly launch those workers directly, with no SSH at all -- correct, faster,
## and immune to how the host resolves its own name.
.localise_hosts <- function(hosts, me = as.character(Sys.info()[["nodename"]])) {
  me_short <- sub("\\..*$", "", me)
  local <- hosts == me | hosts == me_short | sub("\\..*$", "", hosts) == me_short
  hosts[local] <- "localhost"
  hosts
}

## Open a PSOCK cluster over `hosts` (one worker per element, so repeat a host to get several).
##
## Uses parallelly::makeClusterPSOCK rather than parallel::makeCluster for two reasons that are not
## conveniences here, they are what make this work at all:
##
##   * `revtunnel = TRUE` routes the worker's call-back to the coordinator through the SSH connection
##     it already has. stock `parallel::makeCluster()` tells the worker to dial the coordinator by
##     hostname, which HANGS on this cluster: the compute nodes resolve the coordinator's public
##     name via public DNS and get a CDN address rather than its LAN address. The reverse tunnel
##     sidesteps name resolution and any inbound firewall entirely.
##   * `rscript_libs` sets the worker's .libPaths() at startup. A PSOCK worker otherwise starts with
##     the DEFAULT library, so under renv (i.e. inside a targets pipeline) the project library --
##     the one holding landisutils -- is invisible to it.
##
## parallelly is a Suggests, not an Import: multi-node is opt-in, and requiring it for every install
## would add a dependency the single-node path never touches.
.psock_cluster <- function(hosts, rscript, libs = .libPaths(), project = .renv_project()) {
  if (!requireNamespace("parallelly", quietly = TRUE)) {
    stop(
      "multi-node calibration (`cfg$nodes`) needs the 'parallelly' package; ",
      "install it or leave `cfg$nodes` unset to run single-node.",
      call. = FALSE
    )
  }
  ## Under renv, activate the project ON the worker instead of shipping the coordinator's
  ## .libPaths(). renv keys its library path by PLATFORM
  ## (renv/library/linux-ubuntu-<codename>/R-x.y/...), and this cluster is heterogeneous -- the
  ## coordinator can be on a different Ubuntu release than the compute nodes, in which case the
  ## copied path simply does not exist there and every worker reports the package missing. Letting
  ## the worker source renv/activate.R makes it resolve its own platform's library.
  hosts <- .localise_hosts(hosts)
  args <- list(hosts, rscript = rscript, revtunnel = TRUE)
  if (nzchar(project)) {
    ## Must be CHARACTER, not a language object: passing an expression here is accepted silently but
    ## does not take effect, leaving every worker on its default user library with the package
    ## "missing" and no error to explain why.
    args$rscript_startup <- sprintf(
      'setwd("%s"); source("renv/activate.R")',
      gsub('"', '\\\\"', project)
    )
  } else {
    args$rscript_libs <- libs
  }
  tryCatch(do.call(parallelly::makeClusterPSOCK, args), error = function(e) {
    stop(
      "could not open a PSOCK connection to [",
      paste(unique(hosts), collapse = ", "),
      "]: ",
      conditionMessage(e),
      call. = FALSE
    )
  })
}

## Ask each host for its available RAM AND physical core count, by standing up a throwaway
## cluster. Done in R rather than over ssh so the probe uses exactly the transport the real cluster
## will use -- if PSOCK cannot reach a host, that surfaces here, cheaply, instead of after the pools
## are up. Returns a named numeric vector of GiB; hosts that fail to answer are dropped.
.probe_node_capacity <- function(hosts, rscript, outfile = nullfile()) {
  hosts <- unique(hosts)
  cl <- .psock_cluster(hosts, rscript = rscript)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  ## Deliberately self-contained -- reads /proc/meminfo inline rather than calling
  ## landisutils:::.available_ram_gb(). The probe runs BEFORE the real cluster attaches the package,
  ## and a remote worker's default .libPaths() need not contain the coordinator's (renv) library, so
  ## depending on landisutils here turns a missing-library problem into a confusing "object not
  ## found" from deep inside clusterEvalQ. Keep this free of package dependencies.
  res <- parallel::clusterEvalQ(cl, {
    .avail <- tryCatch(
      {
        mi <- readLines("/proc/meminfo", n = 50L)
        v <- NA_real_
        for (.k in c("^MemAvailable:", "^MemTotal:")) {
          .ln <- grep(.k, mi, value = TRUE)
          if (length(.ln) > 0L) {
            v <- as.numeric(sub("\\D*(\\d+).*", "\\1", .ln[1])) / 1024^2 ## kB -> GiB
            break
          }
        }
        v
      },
      error = function(e) NA_real_
    )
    ## PHYSICAL cores, not logical. LANDIS-II is effectively single-threaded, so one container needs
    ## roughly one core -- but SMT siblings share execution units, so counting threads overstates
    ## capacity by 2x and lets a host be booked to ~2x its real throughput. Count distinct
    ## (physical id, core id) pairs; fall back to the logical count only if that is unavailable.
    .cores <- tryCatch(
      {
        ci <- readLines("/proc/cpuinfo")
        pid <- grep("^physical id", ci, value = TRUE)
        cid <- grep("^core id", ci, value = TRUE)
        if (length(pid) && length(pid) == length(cid)) {
          length(unique(paste(pid, cid)))
        } else {
          length(grep("^processor", ci))
        }
      },
      error = function(e) NA_real_
    )
    list(host = as.character(Sys.info()[["nodename"]]), avail = .avail, cores = as.numeric(.cores))
  })
  ## Re-key by the host string the CALLER used: nodename may be an FQDN while cfg$nodes uses a short
  ## name, and every downstream lookup is by the caller's spelling.
  list(
    ram = stats::setNames(vapply(res, function(x) as.numeric(x$avail), numeric(1)), hosts),
    cores = stats::setNames(vapply(res, function(x) as.numeric(x$cores), numeric(1)), hosts)
  )
}

## Cap each host's worker count by its own PHYSICAL core count.
##
## The RAM cap alone is not enough on a heterogeneous cluster. Two hosts can carry the same ~1 TB of
## memory while differing 2.7x in cores (128 vs 48 physical here), so a RAM-only cap admits the same
## number of containers to both and books the smaller one to ~94% of its cores while the larger sits
## at 35%. Because DEoptim waits for every population member, a generation then runs at the pace of
## the saturated host: measured 68 min/rep there against 40 min/rep on the roomy one.
##
## `cores_per_worker` is 1 by default because LANDIS-II is effectively single-threaded -- the docker
## `--cpus` limit is a ceiling, not a reservation, so sizing against it would badly under-fill hosts.
.cap_nodes_by_cpu <- function(nodes, cores, cores_per_worker = 1, cpu_fraction = 0.85) {
  capped <- nodes
  for (h in names(nodes)) {
    n <- cores[[h]]
    if (is.null(n) || !is.finite(n) || n <= 0) {
      next
    }
    cap <- max(1L, as.integer(floor(n * cpu_fraction / max(cores_per_worker, 1e-9))))
    capped[[h]] <- min(nodes[[h]], cap)
  }
  capped
}

## Cap each host's worker count by its own RAM budget. The single-node cap divides one host's budget;
## here each host gets its own, because they differ (1007 vs 976 GiB across this cluster) and a
## coordinator-wide figure would over-subscribe the smaller ones.
.cap_nodes_by_ram <- function(nodes, avail_gb, mem_per_container, mem_fraction) {
  capped <- nodes
  for (h in names(nodes)) {
    a <- avail_gb[[h]]
    if (is.null(a) || !is.finite(a)) {
      next
    }
    capped[[h]] <- .ram_pool_cap(nodes[[h]], mem_per_container, mem_fraction, a)
  }
  capped
}

## Verify every host can run the SAME image before any pool starts.
##
## Two distinct failures, both silent or late without this:
##   * A host missing the image. Pools are started with `pull = FALSE` by default, so the failure
##     surfaces as a container that will not start, after the cluster is already up -- and only on the
##     hosts that lack it, so the run limps along at reduced capacity or dies mid-generation.
##   * Hosts holding DIFFERENT digests behind the same tag. `:ubuntu-24.04` is mutable, so nodes
##     pulled at different times can disagree. Nothing would error: the calibration would simply
##     evaluate some trials against one LANDIS-II build and some against another, and the resulting
##     loss surface would mix them. That is a reproducibility failure, which is worse than a crash
##     because it is invisible in the output.
##
## Returns the common digest invisibly.
## Confirm every worker actually holds a RUNNING container, healing what can be healed and aborting
## loudly on what cannot.
##
## `clusterCall(cl, .worker_pool_start)` reports only hard errors: a container that starts and then
## dies -- OOM, a daemon under load, resources still held by an earlier run's leftovers -- leaves the
## call "successful" and the worker container-less. Every evaluation dispatched to such a worker then
## fails, and because DEoptim scores a failed trial as a penalty rather than an error, the search
## keeps running while a large slice of its population is frozen. Observed on a 90-worker fleet that
## silently came up with 56: half of every generation was wasted for 2.6 days with nothing logged.
##
## Aborting is the point. A fleet that is short of workers is cheap to fix by relaunching and
## ruinously expensive to discover 40 generations later.
.verify_worker_pools <- function(cl, heal = TRUE) {
  ## Probe workers ONE AT A TIME. A cluster-wide clusterCall() is all-or-nothing, so a single
  ## unresponsive worker errors the whole call and hides the state of all the others -- the failure
  ## mode .stop_calibration_cluster() documents having been bitten by twice.
  probe_all <- function() {
    lapply(seq_along(cl), function(i) {
      res <- tryCatch(parallel::clusterCall(cl[i], .worker_pool_probe)[[1L]], error = function(e) {
        NULL
      })
      if (is.null(res)) {
        list(host = NA_character_, container = NA_character_, running = FALSE)
      } else {
        res
      }
    })
  }
  is_bad <- function(info) which(!vapply(info, function(x) isTRUE(x$running), logical(1)))

  info <- probe_all()
  bad <- is_bad(info)

  if (length(bad) && isTRUE(heal)) {
    message(glue::glue(
      "calibrate_dynamic_fire: {length(bad)} of {length(cl)} worker container(s) not running ",
      "after pool start; attempting to restart them."
    ))
    for (i in bad) {
      tryCatch(parallel::clusterCall(cl[i], .worker_pool_heal), error = function(e) NULL)
    }
    info <- probe_all()
    bad <- is_bad(info)
  }

  hosts <- vapply(
    info,
    function(x) {
      h <- x$host
      if (is.null(h) || is.na(h)) NA_character_ else as.character(h)
    },
    character(1)
  )

  if (length(bad)) {
    tally <- table(hosts[bad], useNA = "ifany")
    stop(
      sprintf(
        paste0(
          "%d of %d calibration worker(s) have no running container and could not be restarted ",
          "(by host: %s). The fleet would run degraded -- every trial dispatched to these workers ",
          "fails and is scored as a penalty, freezing that slice of the population. Free the host ",
          "(check for leftover containers from an earlier run: docker ps --filter name=landis-cal-) ",
          "or lower cfg$nodes / cfg$mem_fraction, then relaunch."
        ),
        length(bad),
        length(cl),
        paste(sprintf("%s=%d", names(tally), as.integer(tally)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  ok <- table(hosts)
  message(glue::glue(
    "calibrate_dynamic_fire: all {length(cl)} worker container(s) running ",
    "({paste(sprintf('%s=%d', names(ok), as.integer(ok)), collapse = ', ')})."
  ))
  invisible(info)
}

.verify_node_images <- function(cl, image) {
  if (is.null(image) || !nzchar(image)) {
    return(invisible(NA_character_))
  }
  info <- parallel::clusterCall(
    cl,
    function(img) {
      ## shQuote the format string: system2() pastes args together WITHOUT quoting, so an unquoted
      ## "{{index .RepoDigests 0}}" splits into three shell words and docker exits 64 (usage error) --
      ## which is indistinguishable from a missing image, and would fail every host.
      d <- suppressWarnings(system2(
        "docker",
        c("image", "inspect", "--format", shQuote("{{index .RepoDigests 0}}"), shQuote(img)),
        stdout = TRUE,
        stderr = FALSE
      ))
      list(
        host = as.character(Sys.info()[["nodename"]]),
        digest = if (length(d) && nzchar(d[1])) d[1] else NA_character_
      )
    },
    image
  )
  hosts <- vapply(info, function(x) x$host, character(1))
  digests <- vapply(info, function(x) as.character(x$digest), character(1))

  missing <- is.na(digests)
  if (any(missing)) {
    stop(
      "docker image '",
      image,
      "' is missing on: ",
      paste(unique(hosts[missing]), collapse = ", "),
      ". Pull it there (docker pull ",
      image,
      ") or set cfg$pull = TRUE.",
      call. = FALSE
    )
  }
  uniq <- unique(digests)
  if (length(uniq) > 1L) {
    stop(
      "docker image '",
      image,
      "' resolves to ",
      length(uniq),
      " different digests across hosts, so trials would not be comparable: ",
      paste(sprintf("%s=%s", hosts, substr(digests, 1, 26)), collapse = "; "),
      ". Re-pull so every host agrees.",
      call. = FALSE
    )
  }
  invisible(uniq)
}

## Trim a per-host worker allocation down to `max_workers` total. More workers than DEoptim has
## population members just idle: DEoptim dispatches exactly NP tasks per generation, so worker NP+1
## onwards never receives one while still holding a container and its RAM. Trims from the LAST host
## backwards, so the configured host order decides who keeps capacity; hosts trimmed to zero are
## dropped rather than left as empty entries.
.trim_nodes_to_max <- function(nodes, max_workers) {
  if (!is.finite(max_workers) || sum(nodes) <= max_workers) {
    return(nodes)
  }
  drop <- sum(nodes) - as.integer(max_workers)
  for (h in rev(names(nodes))) {
    if (drop <= 0L) {
      break
    }
    take <- min(drop, nodes[[h]])
    nodes[[h]] <- nodes[[h]] - take
    drop <- drop - take
  }
  nodes[nodes > 0L]
}

## Build the multi-node PSOCK cluster and start one container per worker.
##
## Returns a list(cl, nodes, total) or NULL when `nodes` is not configured. The caller is responsible
## for teardown via .stop_calibration_cluster().
.start_calibration_cluster <- function(
  nodes,
  max_workers,
  image,
  scratch_root,
  cpu_limit,
  mem_limit,
  mem_fraction,
  pull,
  name_prefix,
  rscript = file.path(R.home("bin"), "Rscript"),
  start_pools = TRUE,
  cores_per_worker = 1,
  cpu_fraction = 0.85
) {
  nodes <- .parse_nodes(nodes)
  if (is.null(nodes)) {
    return(NULL)
  }
  mem_per_container <- .mem_limit_to_gb(mem_limit)

  cap_info <- .probe_node_capacity(names(nodes), rscript = rscript)
  avail <- cap_info$ram
  by_ram <- .cap_nodes_by_ram(nodes, avail, mem_per_container, mem_fraction)
  by_cpu <- .cap_nodes_by_cpu(nodes, cap_info$cores, cores_per_worker, cpu_fraction)
  ## Both constraints bind independently: RAM decides how many landscapes fit, physical cores decide
  ## how many can actually run at once. Take whichever is tighter, per host.
  capped <- pmin(by_ram, by_cpu)
  capped <- stats::setNames(as.integer(capped), names(nodes))
  for (h in names(nodes)) {
    if (capped[[h]] < nodes[[h]]) {
      why <- if (by_cpu[[h]] <= by_ram[[h]]) {
        glue::glue("{cap_info$cores[[h]]} physical core(s) x {cpu_fraction} / {cores_per_worker}")
      } else {
        glue::glue(
          "{round(avail[[h]])} GiB avail x {mem_fraction} / {round(mem_per_container, 1)} GiB limit"
        )
      }
      message(glue::glue(
        "calibrate_dynamic_fire: capping {h} {nodes[[h]]} -> {capped[[h]]} worker(s) ({why})."
      ))
    }
  }

  before <- sum(capped)
  capped <- .trim_nodes_to_max(capped, max_workers)
  total <- sum(capped)
  if (total < before) {
    message(glue::glue(
      "calibrate_dynamic_fire: trimming pool {before} -> {total} worker(s) to match NP ",
      "({max_workers}); extra workers would hold containers DEoptim never dispatches to."
    ))
  }
  if (total < 1L) {
    stop(
      "no calibration workers left after RAM capping; check cfg$nodes / mem settings.",
      call. = FALSE
    )
  }

  ## A fleet SMALLER than NP is legitimate, but it is never free, and its cost is a step function
  ## rather than a gradient. DEoptim dispatches NP evaluations per generation and parLapply splits
  ## them into `length(cl)` chunks, so a generation takes ceiling(NP / total) evaluation times and
  ## the barrier waits for whichever worker drew the extra one. Dropping from NP to NP - 1 workers
  ## therefore DOUBLES the generation while leaving almost every worker idle for the second half:
  ## observed here as 89 workers against NP = 90, which turned a 3.9 h generation into ~7.8 h for
  ## want of a single container, with nothing in the log to say so.
  ##
  ## Warn rather than abort: running deliberately below NP is a reasonable choice when a host cannot
  ## hold NP containers, and calibrations do run that way on purpose. The point is that the decision
  ## should be visible and quantified, not discovered from a wall-clock that is quietly 2x.
  if (total < max_workers) {
    waves <- ceiling(max_workers / total)
    idle <- waves * total - max_workers
    warning(
      glue::glue(
        "calibration fleet is {total} worker(s) against NP = {max_workers}: each generation now ",
        "costs {waves} evaluation wave(s) instead of 1",
        if (idle > 0L) {
          glue::glue(", with {idle} worker(s) idle at the barrier in the last wave")
        } else {
          ""
        },
        ". Per host: ",
        paste(sprintf("%s=%d", names(capped), as.integer(capped)), collapse = ", "),
        ". Raise cfg$nodes (or cfg$mem_fraction, if a host was RAM-capped above) to reach ",
        "{max_workers}, or lower NP to {total} to stop paying for a wave that is mostly idle."
      ),
      call. = FALSE
    )
  }

  host_vec <- rep(names(capped), times = as.integer(capped))
  message(glue::glue(
    "calibrate_dynamic_fire: PSOCK cluster over {length(capped)} host(s), {total} worker(s): ",
    "{paste(sprintf('%s=%d', names(capped), as.integer(capped)), collapse=', ')}"
  ))
  cl <- .psock_cluster(host_vec, rscript = rscript)

  ok <- FALSE
  on.exit(if (!ok) try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  ## PSOCK workers start clean: unlike FORK they inherit nothing, so the package must be attached
  ## before any worker-side landisutils call. (.libPaths() was already set at worker startup via
  ## rscript_libs in .psock_cluster().)
  attached <- unlist(parallel::clusterEvalQ(cl, requireNamespace("landisutils", quietly = TRUE)))
  if (!all(attached)) {
    stop(
      "landisutils is not loadable on ",
      sum(!attached),
      " of ",
      length(attached),
      " worker(s). The project library must exist at the same path on every host ",
      "(check `scripts/sync-nodes.R` has run and the renv library is present).",
      call. = FALSE
    )
  }
  parallel::clusterEvalQ(cl, library(landisutils))
  if (isTRUE(start_pools)) {
    ## Fail fast and loudly if the hosts cannot agree on the image, BEFORE any container starts.
    digest <- .verify_node_images(cl, image)
    if (!is.na(digest)) {
      message(glue::glue(
        "calibrate_dynamic_fire: image digest {substr(digest, 1, 26)} on all hosts"
      ))
    }
    ## Pass the function OBJECT rather than wrapping a `landisutils:::` call: clusterCall serialises
    ## the closure (its namespace resolves on the worker, which has the package attached), so this
    ## needs no ::: into our own namespace -- which R CMD check flags, rightly.
    parallel::clusterCall(
      cl,
      .worker_pool_start,
      image = image,
      scratch_root = scratch_root,
      cpu_limit = cpu_limit,
      mem_limit = mem_limit,
      pull = pull,
      name_prefix = name_prefix
    )
    ## clusterCall() above surfaces only hard errors. Containers that started and then died are
    ## invisible to it (and, under `--rm`, to `docker ps -a` too), so verify the fleet is at full
    ## strength before handing it to DEoptim.
    .verify_worker_pools(cl)
  }
  ok <- TRUE

  list(cl = cl, nodes = capped, total = total)
}

## Tear down worker pools FIRST, then the cluster: stopping the cluster first would orphan every
## container, since only the worker knows its own container's name.
.stop_calibration_cluster <- function(mc) {
  if (is.null(mc)) {
    return(invisible(NULL))
  }
  n <- length(mc$cl)

  ## Stop each worker's pool INDEPENDENTLY. A single `clusterCall()` over the whole cluster is
  ## all-or-nothing: it dispatches to every node and waits for every reply, so one unresponsive
  ## worker errors the entire call -- and wrapped in `try(silent = TRUE)` that left EVERY
  ## container running with nothing reported. Observed twice on a 45-worker fleet, once after a
  ## deliberate stop and once after a clean completion; losing all 45 rather than a subset is the
  ## signature of the single call failing wholesale.
  failed <- integer(0)
  for (i in seq_len(n)) {
    res <- tryCatch(parallel::clusterCall(mc$cl[i], .worker_pool_stop), error = function(e) e)
    if (inherits(res, "error")) {
      failed <- c(failed, i)
    }
  }
  if (length(failed)) {
    warning(
      sprintf(
        paste0(
          "%d of %d calibration worker pool(s) could not be torn down; their containers are ",
          "still running and must be removed by hand (docker ps --filter name=landis-). ",
          "Unreachable worker index(es): %s."
        ),
        length(failed),
        n,
        paste(failed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  try(parallel::stopCluster(mc$cl), silent = TRUE)
  invisible(list(stopped = n - length(failed), failed = length(failed)))
}
