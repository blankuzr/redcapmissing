#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the validation summary from a `redcapmissing` report as a
#' `flextable` for reporting workflows. Unlike `summary()`, this function is
#' presentation-focused and formats context-stratified pass/fail counts for
#' display.
#'
#' @param x An object to format.
#' @param ... Unused.
#'
#' @return A `flextable` object.
#'
#' @export
flex <- function(x, ...) {
  UseMethod("flex")
}

#' @export
flex.redcapmissing <- function(x, ...) {
  .redcapmissing_check_report(x)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex()")

  validation_set <- x$agent$validation_set
  if (!"validation_context" %in% names(validation_set)) {
    validation_set$validation_context <- "overall"
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
