# Internal report and dependency checks shared by accessors and reporting helpers.

.redcapmissing_check_report <- function(x, arg = "x") {
  if (!inherits(x, "redcapmissing")) {
    .rcm_plan_abort(
      paste0(
        "`", arg,
        "` must be a `redcapmissing` object created by `run_plan()`."
      ),
      "argument"
    )
  }
  expected_names <- c(
    "plan", "target_results", "summary", "missing", "verification",
    "diagnostics", "details"
  )
  if (!is.list(x) || !identical(names(x), expected_names)) {
    .rcm_plan_abort(
      paste0(
        "`", arg, "` must contain exactly: ",
        paste(expected_names, collapse = ", "), "."
      ),
      "schema"
    )
  }
  .rcm_validate_plan(x$plan)
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