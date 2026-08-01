## Shared docker gate for every docker-backed test.
##
## Lives in a helper file, not in one test file, because testthat scopes a test file's top-level
## definitions to that file: a copy in test-landis_pool.R was invisible to the other docker tests,
## which then rolled their own weaker `Sys.which("docker")` checks and failed on Windows CI.
.docker_available <- function() {
  ## These tests use a Linux base image (busybox). Windows CI runners typically have the docker CLI
  ## present but configured for Windows containers ONLY, so a Linux image cannot be pulled, inspected
  ## or run there -- `docker run busybox:latest` fails with exit status 125. A `Sys.which("docker")`
  ## check passes on such a runner and the test then fails for a reason that has nothing to do with
  ## the package. Skip instead of trying.
  if (.Platform$OS.type == "windows") {
    return(FALSE)
  }
  rc <- suppressWarnings(system2("docker", "version", stdout = FALSE, stderr = FALSE))
  identical(as.integer(rc), 0L)
}
