#' Get unresolved missing rows from a REDCap missingness report
#'
#' `get_missing()` returns effective unresolved failures stored by [run_plan()].
#' Verification-applied failures are absent. Filters subset the completed report
#' without rerunning checks or changing denominators.
#'
#' @inheritParams get_summary
#'
#' @return A tibble with exactly these columns and storage types:
#'
#' | Column | Storage and meaning |
#' |---|---|
#' | `record_id` | Character canonical record ID |
#' | `redcap_event_name` | Character raw event name; `NA_character_` in classic projects |
#' | `repeat_instrument` | Character raw repeating instrument; otherwise `NA_character_` |
#' | `repeat_instance` | Integer exact instance; otherwise `NA_integer_` |
#' | `validation_context` | Character display context for the event/repeat location |
#' | `instrument` | Character raw instrument name |
#' | `validation_check` | Character canonical validation-check code from [registry()] |
#' | `field_name`, `field_label`, `field_type`, `branching_logic` | Character field context; typed missing for failures not tied to one field |
#' | `url` | Character REDCap data-entry URL when it can be constructed; otherwise `NA_character_` |
#'
#' Structural absence is represented by typed missing values, never blank-string
#' placeholders. The tibble has a `redcapmissing_labels` attribute containing
#' named character vectors `events` and `instruments` for presentation; raw
#' values remain the filtering and data contract.
#'
#' @examples
#' \dontrun{
#' plan <- plan_from_data(records, rcon, "baseline")
#' report <- run_plan(plan, records, rcon)
#'
#' get_missing(report)
#' get_missing(report, validation_check = "field-complete")
#' get_missing(report, instruments = "baseline")
#' }
#'
#' @seealso [get_summary()], [run_plan()], [registry()], [flexify()]
#'
#' @export
get_missing <- function(
  report,
  validation_check = NULL,
  events = NULL,
  instruments = NULL
) {
  .redcapmissing_check_report(report, arg = "report")
  .redcapmissing_check_missing_rows(report$missing)

  filters <- .redcapmissing_resolve_accessor_filters(
    report = report,
    validation_check = validation_check,
    events = events,
    instruments = instruments
  )
  missing_rows <- .redcapmissing_filter_accessor_rows(
    rows = report$missing,
    filters = filters
  )
  out <- tibble::as_tibble(missing_rows)

  .redcapmissing_attach_labels(out, report)
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_get_missing_columns <- function() {
  c(
    "record_id",
    "redcap_event_name",
    "repeat_instrument",
    "repeat_instance",
    "validation_context",
    "instrument",
    "validation_check",
    "field_name",
    "field_label",
    "field_type",
    "branching_logic",
    "url"
  )
}

.redcapmissing_get_missing_prototype <- function() {
  tibble::tibble(
    record_id = character(),
    redcap_event_name = character(),
    repeat_instrument = character(),
    repeat_instance = integer(),
    validation_context = character(),
    instrument = character(),
    validation_check = character(),
    field_name = character(),
    field_label = character(),
    field_type = character(),
    branching_logic = character(),
    url = character()
  )
}

.redcapmissing_check_missing_rows <- function(missing_rows) {
  .redcapmissing_check_report_rows(
    rows = missing_rows,
    expected = .redcapmissing_get_missing_prototype(),
    component = "missing"
  )
  invalid_checks <- setdiff(unique(missing_rows$validation_check), .redcapmissing_validation_checks())
  if (length(invalid_checks) > 0L) {
    stop("`report$missing` contains unknown validation-check codes.", call. = FALSE)
  }
  invisible(missing_rows)
}