#' Inspect the redcapmissing validation registry
#'
#' @description
#' `registry()` returns the validation taxonomy used by [find_missing()].
#' The registry is the package source of truth for validation levels,
#' validation checks, check types, display labels, downstream order, and the
#' internal R-safe stems used to build report components.
#'
#' @return A tibble with class `"redcapmissing_registry"` and one row per
#'   validation check. The returned columns include:
#' \describe{
#'   \item{`validation_order`}{The canonical assessment order.}
#'   \item{`downstream_order`}{The order used when applying downstream gating.}
#'   \item{`validation_level`}{The validation level: `"row"`, `"form"`, or
#'     `"field"`.}
#'   \item{`validation_check`}{The canonical validation-check code.}
#'   \item{`validation_check_type`}{The check type: `"on-route"` or
#'     `"detour"`.}
#'   \item{`validation_label`}{The canonical pointblank label.}
#'   \item{`flex_label`}{The display label used by [flex()].}
#'   \item{`description`}{A short user-facing description of the check.}
#'   \item{`r_identifier`, `component_stem`, `step_suffix`}{Internal R-safe
#'     stems and pointblank step metadata.}
#'   \item{`gates_downstream`}{Whether a failed check removes that context from
#'     downstream assessment.}
#' }
#'
#' @examples
#' registry()
#'
#' @export
registry <- function() {
  .redcapmissing_new_registry(.redcapmissing_registry_data())
}

#' @export
print.redcapmissing_registry <- function(x, ...) {
  cli::cli_h1("redcapmissing validation registry")

  for (level in unique(x$validation_level)) {
    level_rows <- x[x$validation_level == level, , drop = FALSE]
    cli::cli_h2("{level}-level checks")
    for (row in seq_len(nrow(level_rows))) {
      check <- level_rows[row, , drop = FALSE]
      gate_text <- if (isTRUE(check$gates_downstream)) {
        "gates downstream"
      } else {
        "does not gate downstream"
      }
      cli::cli_li(
        "{.code {check$validation_check}} [{check$validation_check_type}; {gate_text}] - {check$description}"
      )
    }
  }

  invisible(x)
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_registry_data <- function() {
  tibble::tibble(
    validation_order = 1:5,
    downstream_order = 1:5,
    validation_level = c("row", "row", "form", "form", "field"),
    validation_check = c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "form-complete",
      "field-complete"
    ),
    validation_check_type = c(
      "on-route",
      "on-route",
      "on-route",
      "detour",
      "on-route"
    ),
    validation_label = c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "form-complete",
      "field-complete"
    ),
    flex_label = c(
      "Event row started",
      "Instance row started",
      "Form started",
      "Form complete",
      "Field complete"
    ),
    description = c(
      "The expected REDCap event row exists in the export.",
      "The expected REDCap repeat instance row exists in the export.",
      "The exported form context has at least one entered data-capturing field.",
      "All expected fields are complete for an evaluable form context.",
      "A specific expected field is complete after branching and filtering."
    ),
    r_identifier = c(
      "event_row_started",
      "instance_row_started",
      "form_started",
      "form_complete",
      "field_complete"
    ),
    component_stem = c(
      "event_row_started",
      "instance_row_started",
      "form_started",
      "form_complete",
      "field_complete"
    ),
    step_suffix = c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "form-complete",
      "field-complete"
    ),
    gates_downstream = c(TRUE, TRUE, TRUE, FALSE, TRUE)
  )
}

.redcapmissing_new_registry <- function(x) {
  class(x) <- c("redcapmissing_registry", class(x))
  x
}

.redcapmissing_registry_row <- function(validation_check) {
  registry <- .redcapmissing_registry_data()
  out <- registry[registry$validation_check == validation_check, , drop = FALSE]
  if (nrow(out) != 1) {
    stop(
      "Unknown validation check `",
      validation_check,
      "`.",
      call. = FALSE
    )
  }
  out
}

.redcapmissing_validation_checks <- function() {
  .redcapmissing_registry_data()$validation_check
}

.redcapmissing_on_route_checks <- function() {
  registry <- .redcapmissing_registry_data()
  registry$validation_check[registry$gates_downstream]
}

.redcapmissing_validation_metadata <- function(validation_check, n) {
  check <- .redcapmissing_registry_row(validation_check)
  tibble::tibble(
    validation_level = rep(check$validation_level, n),
    validation_check_type = rep(check$validation_check_type, n),
    validation_check = rep(check$validation_check, n),
    validation_label = rep(check$validation_label, n)
  )
}

.redcapmissing_flex_labels <- function(validation_check) {
  registry <- .redcapmissing_registry_data()
  flex_label <- registry$flex_label[
    match(validation_check, registry$validation_check)
  ]
  flex_label[is.na(flex_label)] <- validation_check[is.na(flex_label)]
  flex_label
}
