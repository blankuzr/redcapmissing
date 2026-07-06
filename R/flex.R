#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the tidy validation summary from a REDCap missingness
#' report as a `flextable` for reporting workflows.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Unused.
#'
#' @return A `flextable` object with form metadata, validation level,
#'   validation-check type, human-readable validation check, REDCap context
#'   columns, and pass/fail counts for display. This function requires the
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
      "validation_level",
      "validation_check_type",
      "validation_check",
      "redcap_event_name",
      "redcap_repeat_instrument",
      "redcap_repeat_instance",
      "assessed",
      "passed",
      "failed"
    ))) |>
    stats::setNames(c(
      "Form",
      "Form Label",
      "Validation Level",
      "Check Type",
      "Validation Check",
      "Event",
      "Repeat Instrument",
      "Repeat Instance",
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
