## Score a calibration's stored validation payload under a CANDIDATE landisutils commit, beside the
## installed version, without touching the project library, its lockfile, or any checkout.
##
## Use it to see what a landisutils change does to a calibration's loss BEFORE adopting it. It
## prints the loss components under both versions, flags which moved, and returns both. No LANDIS-II
## runs: the replicates already stored by `run_calibration_validation()` are rescored.
##
## Usage (from the calibrating project's root, with the project's renv active):
##
##   candidate_ref <- "fix/some-branch"   ## branch, tag, or FULL 40-character sha
##   res <- source(system.file("scripts/verify_candidate_loss.R", package = "landisutils"))$value
##
## Optional, set before sourcing:
##
##   stats_target   <- "calibration_validation_stats"  ## target holding the validation result
##   candidate_repo <- "https://github.com/..."        ## default: where landisutils was installed from
##
## WHY THIS SHAPE. Two properties matter and are easy to lose:
##
##   * ISOLATION. The candidate is fetched into a temporary clone and installed into a temporary
##     library. Installing it into the project library would move the project to an unreviewed
##     version for every later session, and moving a submodule pointer would dirty a checkout that
##     other sessions may share. Dependencies come read-only from the current library paths, so only
##     landisutils is swapped.
##   * SEPARATE PROCESSES. Each version is scored in its own R process, with no profile read. Two
##     versions of one package cannot be loaded into one session, and a namespace that is already
##     loaded silently wins. Each child reports where it loaded landisutils from, and the script
##     stops if the candidate did not come from the temporary library.
##
## What to read beyond the headline total: components the change should NOT touch should be
## identical (compared at tolerance 0). A component drifting that the change should not affect is the
## signal that something else came along with it.
##
## Cost: one source install of landisutils plus two rescorings of the stored replicates. Run it on a
## compute node rather than a small control node.

local({
  if (
    !exists("candidate_ref") ||
      !is.character(candidate_ref) ||
      length(candidate_ref) != 1L ||
      !nzchar(candidate_ref)
  ) {
    stop(
      "Set `candidate_ref` (a landisutils branch, tag or full sha) before sourcing this script.",
      call. = FALSE
    )
  }
  ## A shallow fetch can name a commit only by its full id, so an abbreviated sha would fail inside
  ## git with a message about the remote rather than about the argument.
  if (grepl("^[0-9a-f]{7,39}$", candidate_ref)) {
    stop(
      "`candidate_ref` looks like an abbreviated sha; give the full 40-character sha.",
      call. = FALSE
    )
  }
  for (pkg in c("callr", "targets")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("`", pkg, "` is required to run this script.", call. = FALSE)
    }
  }
  if (!nzchar(Sys.which("git"))) {
    stop("`git` must be on the PATH.", call. = FALSE)
  }

  stats_target <- if (exists("stats_target")) stats_target else "calibration_validation_stats"
  repo <- if (exists("candidate_repo")) {
    candidate_repo
  } else {
    desc <- utils::packageDescription("landisutils")
    if (is.null(desc$RemoteUsername) || is.null(desc$RemoteRepo)) {
      stop(
        "The installed landisutils records no GitHub remote; set `candidate_repo`.",
        call. = FALSE
      )
    }
    sprintf("https://github.com/%s/%s.git", desc$RemoteUsername, desc$RemoteRepo)
  }

  work <- tempfile("verify_candidate_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  git <- function(...) {
    out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(out, "status"))) {
      stop("git ", paste(c(...), collapse = " "), " failed:\n", paste(out, collapse = "\n"))
    }
    out
  }

  message("fetching ", candidate_ref, " from ", repo)
  src <- file.path(work, "src")
  git("init", "--quiet", src)
  git("-C", src, "fetch", "--quiet", "--depth", "1", repo, candidate_ref)
  git("-C", src, "checkout", "--quiet", "FETCH_HEAD")
  sha <- git("-C", src, "rev-parse", "HEAD")

  message(
    "installing landisutils ",
    read.dcf(file.path(src, "DESCRIPTION"), "Version"),
    " into a temporary library"
  )
  lib <- file.path(work, "lib")
  dir.create(lib)
  inst <- callr::rcmd(
    "INSTALL",
    c("--no-docs", "--no-multiarch", "--no-byte-compile", paste0("--library=", lib), src),
    libpath = .libPaths(),
    wd = work,
    user_profile = FALSE,
    fail_on_status = FALSE
  )
  if (inst$status != 0L) {
    stop(
      "R CMD INSTALL of the candidate failed:\n",
      paste(utils::tail(strsplit(paste(inst$stdout, inst$stderr), "\n")[[1]], 20L), collapse = "\n"),
      call. = FALSE
    )
  }

  val <- targets::tar_read_raw(stats_target)
  if (is.null(val$reps) || is.null(val$observed) || is.null(val$loss$weights)) {
    stop(
      "`",
      stats_target,
      "` does not look like a run_calibration_validation() result ",
      "(needs `reps`, `observed` and `loss$weights`).",
      call. = FALSE
    )
  }

  score <- function(libpath) {
    callr::r(
      function(reps, observed, weights) {
        loss <- landisutils::loss_from_stats(reps, observed, weights = weights)
        list(
          version = as.character(utils::packageVersion("landisutils")),
          lib = dirname(getNamespaceInfo("landisutils", "path")),
          components = loss$components,
          weights = loss$weights,
          total = loss$total
        )
      },
      args = list(val$reps, val$observed, val$loss$weights),
      libpath = libpath,
      wd = work,
      user_profile = FALSE
    )
  }

  message("scoring under the installed version and under the candidate, in separate processes")
  installed <- score(.libPaths())
  candidate <- score(c(lib, .libPaths()))
  if (!identical(normalizePath(candidate$lib), normalizePath(lib))) {
    stop(
      "the candidate was scored with landisutils from ",
      candidate$lib,
      ", not from the temporary library; the comparison is void.",
      call. = FALSE
    )
  }

  k <- union(names(installed$components), names(candidate$components))
  comparison <- data.frame(
    component = k,
    weight = unname(installed$weights[k]),
    installed = unname(installed$components[k]),
    candidate = unname(candidate$components[k]),
    identical = vapply(
      k,
      \(i) isTRUE(all.equal(installed$components[i], candidate$components[i], tolerance = 0)),
      logical(1)
    ),
    row.names = NULL
  )

  shown <- comparison
  shown$installed <- signif(shown$installed, 6)
  shown$candidate <- signif(shown$candidate, 6)
  cat(sprintf(
    "\ninstalled %s   candidate %s (%s @ %s)\n\n",
    installed$version,
    candidate$version,
    candidate_ref,
    substr(sha, 1L, 12L)
  ))
  print(shown)
  cat(sprintf(
    "\n  weighted total %.6f -> %.6f  (delta %+.6f)\n",
    installed$total,
    candidate$total,
    candidate$total - installed$total
  ))
  moved <- comparison$component[!comparison$identical]
  cat("  moved:     ", if (length(moved)) paste(moved, collapse = ", ") else "(none)", "\n")
  cat("  identical: ", paste(comparison$component[comparison$identical], collapse = ", "), "\n")

  invisible(list(
    candidate_ref = candidate_ref,
    candidate_sha = sha,
    installed = installed,
    candidate = candidate,
    comparison = comparison
  ))
})
