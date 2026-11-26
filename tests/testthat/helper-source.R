## Source-inspection tests deparse a function body and regex-match it. Under
## covr::package_coverage() that body is instrumented: covr wraps each statement and each branch as
## `if (TRUE) { .Call(covr:::covr_count, "<key>"); <expr> }` (covr >= 4.0.0) or as
## `{ covr:::count("<key>"); <expr> }` (earlier releases), which puts counter calls between tokens
## the patterns expect to be adjacent. `.strip_covr()` removes exactly those wrappers, so the tests
## match the code as written whether or not coverage is being measured. On uninstrumented code it
## returns an identical expression.
.is_covr_counter <- function(x) {
  is.call(x) &&
    (identical(x[[1L]], quote(covr:::count)) ||
      (identical(x[[1L]], quote(.Call)) &&
        length(x) >= 2L &&
        identical(x[[2L]], quote(covr:::covr_count))))
}

.is_covr_block <- function(x) {
  is.call(x) && identical(x[[1L]], as.name("{")) && length(x) == 3L && .is_covr_counter(x[[2L]])
}

## calls, and the formals of a nested `function` definition; not NULL, which is also a pairlist
.is_lang_node <- function(x) {
  is.call(x) || (is.pairlist(x) && !is.null(x))
}

.strip_covr <- function(x) {
  if (!.is_lang_node(x)) {
    return(x)
  }
  if (is.call(x)) {
    if (
      identical(x[[1L]], as.name("if")) &&
        length(x) == 3L &&
        isTRUE(x[[2L]]) &&
        .is_covr_block(x[[3L]])
    ) {
      return(.strip_covr(x[[3L]][[3L]]))
    }
    if (.is_covr_block(x)) {
      return(.strip_covr(x[[3L]]))
    }
  }
  ## Rebuild from a list: elements can be the empty symbol (a formal without a default, or the
  ## gap in `x[, 1]`), which cannot be bound to a variable, and `[[<-` with NULL drops the element.
  parts <- as.list(x)
  for (i in seq_along(parts)) {
    if (.is_lang_node(parts[[i]])) {
      parts[i] <- list(.strip_covr(parts[[i]]))
    }
  }
  if (is.call(x)) as.call(parts) else as.pairlist(parts)
}

## `deparse(body(fn))` with any coverage instrumentation removed first.
.body_src <- function(fn) {
  deparse(.strip_covr(body(fn)))
}
