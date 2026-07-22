#' Get focused missing rows from a REDCap missingness report
#'
#' @description
#' `get_missing()` returns the user-facing failed validation rows from a
#' [find_missing()] report. By default, it returns failures from every report
#' context and validation check.
#'
#' @details
#' Filtering follows [get_summary()]: raw, case-sensitive filters are combined
#' by intersection and only subset the completed report. Event and form values
#' are validated against its configured scope, and validation checks against
#' [registry()]. Valid filters with no failures return a zero-row tibble with
#' the documented schema.
#'
#' Output preserves the failure-row order from the completed report.
#'
#' @inheritParams get_summary
#'
#' @return A tibble containing failed validation rows with these columns:
#' \describe{
#'   \item{`record_id`}{The REDCap record identifier.}
#'   \item{`redcap_event_name`}{The raw REDCap event name, or `""` when event
#'     context is not applicable.}
#'   \item{`redcap_repeat_instrument`}{The raw REDCap repeat instrument, or
#'     `""` when repeat-instrument context is not applicable.}
#'   \item{`redcap_repeat_instance`}{The raw REDCap repeat instance, or `""`
#'     when repeat-instance context is not applicable.}
#'   \item{`validation_context`}{The overall, event, or repeat-instance
#'     context for the failed validation row.}
#'   \item{`form`}{The REDCap instrument/form name.}
#'   \item{`validation_check`}{The canonical validation-check code.}
#'   \item{`field_name`}{The REDCap field name, or `NA` when the check is
#'     not field-specific.}
#'   \item{`field_label`}{The REDCap field label, or `NA` when the check is
#'     not field-specific.}
#'   \item{`field_type`}{The REDCap field type, or `NA` when the check is
#'     not field-specific.}
#'   \item{`branching_logic`}{The field branching logic, or `NA` when the
#'     check is not field-specific.}
#'   \item{`url`}{A raw REDCap Data Entry URL for the failed record/form
#'     context when available; otherwise `NA`.}
#' }
#'
#' All returned columns use character storage. The tibble carries a
#' `redcapmissing_labels` attribute containing named `events` and `forms`
#' character vectors for presentation. Data manipulation may drop this
#' optional metadata; raw context values remain in the columns.
#'
#' @examples
#' \dontrun{
#' report <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = "baseline_form"
#' )
#'
#' get_missing(report)
#' get_missing(report, validation_check = "field-complete")
#' get_missing(report, events = "baseline_event", forms = "baseline_form")
#' }
#'
#' @seealso [get_summary()], [find_missing()], [registry()], [flexify()]
#'
#' @export
get_missing <- function(
  report,
  validation_check = NULL,
  events = NULL,
  forms = NULL
) {
  .redcapmissing_check_report(report, arg = "report")
  .redcapmissing_check_missing_rows(report$missing)

  filters <- .redcapmissing_resolve_accessor_filters(
    report = report,
    validation_check = validation_check,
    events = events,
    forms = forms
  )
  missing_rows <- .redcapmissing_filter_accessor_rows(
    rows = report$missing,
    filters = filters
  )
  out <- tibble::as_tibble(missing_rows[
    ,
    .redcapmissing_get_missing_columns(),
    drop = FALSE
  ])

  .redcapmissing_attach_labels(out, report)
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_get_missing_columns <- function() {
  c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_context",
    "form",
    "validation_check",
    "field_name",
    "field_label",
    "field_type",
    "branching_logic",
    "url"
  )
}

.redcapmissing_get_missing_prototype <- function() {
  expected <- .miss_empty_missing_rows()
  expected[, .redcapmissing_get_missing_columns(), drop = FALSE]
}

.redcapmissing_check_missing_rows <- function(missing_rows) {
  .redcapmissing_check_report_rows(
    rows = missing_rows,
    expected = .miss_empty_missing_rows(),
    component = "missing"
  )
}
