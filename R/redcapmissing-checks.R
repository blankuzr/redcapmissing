# Internal report and dependency checks shared by tidy and flex helpers.

.redcapmissing_check_report <- function(x, arg = "x") {
  if (!inherits(x, "redcapmissing")) {
    stop(
      "`",
      arg,
      "` must be a `redcapmissing` object created by `find_missing()`.",
      call. = FALSE
    )
  }
  if (is.null(x$agent) || is.null(x$agent$validation_set)) {
    stop(
      "`",
      arg,
      "` must contain `agent$validation_set`.",
      call. = FALSE
    )
  }
  invisible(x)
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
