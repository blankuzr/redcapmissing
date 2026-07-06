#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the tidy validation summary from a REDCap missingness
#' report as a `flextable` for reporting workflows.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Unused.
#'
#' @return A `flextable` object with form metadata, REDCap context columns,
#'   validation level, human-readable validation check, validation-check type,
#'   and pass/fail counts for display. This function requires the
#'   optional `flextable` and `glue` packages.
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

  validation_set$validation_check <- .redcapmissing_flex_labels(
    validation_set$validation_check
  )
  validation_set$passed <- glue::glue(
    "{validation_set$passed} ({round(validation_set$pass_rate * 100, 1)}%)"
  )
  validation_set$failed <- glue::glue(
    "{validation_set$failed} ({round(validation_set$fail_rate * 100, 1)}%)"
  )

  validation_set |>
    dplyr::select(dplyr::all_of(c(
      "form",
      "form_label",
      "redcap_event_name",
      "redcap_repeat_instrument",
      "redcap_repeat_instance",
      "validation_level",
      "validation_check",
      "validation_check_type",
      "assessed",
      "passed",
      "failed"
    ))) |>
    stats::setNames(c(
      "Form",
      "Form Label",
      "Event",
      "Repeat Instrument",
      "Repeat Instance",
      "Validation Level",
      "Validation Check",
      "Check Type",
      "Assessed",
      "Passed",
      "Failed"
    )) |>
    flextable::flextable() |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}
