#' Get focused missing rows from a REDCap missingness report
#'
#' @description
#' `get_missing()` returns the user-facing failed validation rows from a
#' [find_missing()] report. By default, it returns failures from every report
#' context and validation check.
#'
#' @details
#' Row-started and form-started failures do not describe an individual field,
#' so their field metadata columns contain `NA`. The `url` column remains a raw
#' REDCap Data Entry URL when one is available and otherwise contains `NA`.
#'
#' Filters use raw, case-sensitive values and are combined by intersection.
#' They only subset the completed report; they do not rerun validation.
#' Duplicate filter values are treated as a set. Event and form filters are
#' validated against the report's configured scope, including configured
#' contexts with no failed rows. Validation checks are validated against
#' [registry()]. A valid filter, or combination of filters, with no matching
#' failures returns a zero-row tibble with the documented schema.
#'
#' `get_missing()` preserves the row order and values stored in
#' `report$missing`.
#'
#' @param report A `redcapmissing` object created by [find_missing()].
#' @param validation_check `NULL`, or a non-empty character vector containing
#'   raw, canonical validation-check codes from [registry()]. `NULL` keeps all
#'   checks.
#' @param events `NULL`, or a non-empty character vector containing raw REDCap
#'   `redcap_event_name` values configured in the report. `NULL` keeps all
#'   events.
#' @param forms `NULL`, or a non-empty character vector containing raw REDCap
#'   instrument/form names configured in the report. `NULL` keeps all forms.
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
