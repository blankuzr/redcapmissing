# Internal report and dependency checks shared by accessors and reporting helpers.

.redcapmissing_check_report <- function(x, arg = "x") {
  if (!inherits(x, "redcapmissing")) {
    stop(
      "`",
      arg,
      "` must be a `redcapmissing` object created by `find_missing()`.",
      call. = FALSE
    )
  }
  if (is.null(x$summary)) {
    stop(
      "`",
      arg,
      "` must contain `summary`.",
      call. = FALSE
    )
  }
  invisible(x)
}

.redcapmissing_report_spec <- function(x) {
  x$spec %||% list()
}

.redcapmissing_check_packages <- function(packages, context) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0) {
    stop(
      "Install the following package(s) before using `",
      context,
      "`: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(packages)
}
