#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the validation summary from a REDCap missingness report as a
#' `flextable` for reporting workflows. It accepts either a `redcapmissing`
#' object returned by [find_missing()] or the `"summary.redcapmissing"` object
#' returned by `summary()` for that report. Calling `flex(report)` is equivalent
#' to formatting `summary(report)`.
#'
#' @param x A `redcapmissing` object created by [find_missing()], or a
#'   `"summary.redcapmissing"` object returned by `summary()` for a
#'   `redcapmissing` report.
#' @param ... Unused.
#'
#' @return A `flextable` object with context-stratified pass/fail counts for
#'   display. This function requires the optional `flextable` and `glue`
#'   packages.
#'
#' @seealso [find_missing()], [summary.redcapmissing()], [flex_html()]
#'
#' @export
flex <- function(x, ...) {
  UseMethod("flex")
}

#' @export
flex.redcapmissing <- function(x, ...) {
  .redcapmissing_check_report(x)
  flex(summary(x), ...)
}

#' @export
flex.summary.redcapmissing <- function(x, ...) {
  .redcapmissing_check_packages(c("flextable", "glue"), "flex()")

  validation_set <- x
  required_columns <- c(
    "label",
    "validation_context",
    "n",
    "n_passed",
    "n_failed",
    "f_passed",
    "f_failed"
  )
  missing_columns <- setdiff(required_columns, names(validation_set))
  if (length(missing_columns) > 0) {
    stop(
      "`x` must include the current `summary.redcapmissing` columns: ",
      paste(required_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  validation_set |>
    dplyr::mutate(
      n_passed = glue::glue("{n_passed} ({round(f_passed * 100, 1)}%)"),
      n_failed = glue::glue("{n_failed} ({round(f_failed * 100, 1)}%)")
    ) |>
    dplyr::select(dplyr::all_of(c(
      "label",
      "validation_context",
      "n",
      "n_passed",
      "n_failed"
    ))) |>
    stats::setNames(c("Evaluation", "Context", "Assessed", "Passed", "Failed")) |>
    flextable::flextable() |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}
