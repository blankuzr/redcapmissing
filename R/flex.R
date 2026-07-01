#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the tidy validation summary from a REDCap missingness
#' report as a `flextable` for reporting workflows.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Unused.
#'
#' @return A `flextable` object with context-stratified pass/fail counts for
#'   display. This function requires the optional `flextable` and `glue`
#'   packages.
#'
#' @seealso [find_missing()], [tidy.redcapmissing()], [flex_html()]
#'
#' @export
flex <- function(x, ...) {
  UseMethod("flex")
}

#' @export
flex.redcapmissing <- function(x, ...) {
  .redcapmissing_check_report(x)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex()")

  validation_set <- generics::tidy(x)

  validation_set |>
    dplyr::mutate(
      passed = glue::glue("{passed} ({round(pass_rate * 100, 1)}%)"),
      failed = glue::glue("{failed} ({round(fail_rate * 100, 1)}%)")
    ) |>
    dplyr::select(dplyr::all_of(c(
      "validation",
      "validation_context",
      "assessed",
      "passed",
      "failed"
    ))) |>
    stats::setNames(c("Evaluation", "Context", "Assessed", "Passed", "Failed")) |>
    flextable::flextable() |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}
