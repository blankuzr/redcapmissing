#' Get validation summaries from a REDCap missingness report
#'
#' `get_summary()` returns the stored validation summary from a report created by
#' [run_plan()]. Filters select rows from the stored summary. Assessment results,
#' applicability, and denominators remain those computed by [run_plan()].
#'
#' @param report A `redcapmissing` report from [run_plan()] or a
#'   `redcapmissing_comparison` from [compare_reports()].
#' @param ... Arguments for methods. The comparison method accepts `population`.
#' @param validation_check `NULL`, or a nonempty character vector containing one
#'   or more exact codes from [registry()]: `"event-row-started"`,
#'   `"repeat-instance-row-started"`, `"instrument-started"`, or
#'   `"field-complete"`.
#' @param events `NULL`, or a nonempty character vector of exact raw REDCap
#'   unique event names represented by the plan. Classic project event context
#'   is `NA_character_`; filters contain raw event names.
#' @param instruments `NULL`, or a nonempty character vector of exact raw REDCap
#'   instrument names selected by the plan.
#'
#' @section Filter semantics:
#' Values within one non-`NULL` filter vector are alternatives: a stored row
#' matches that filter when its exact raw value equals any supplied value.
#' Multiple non-`NULL` filters are combined by intersection, so a row must
#' satisfy every supplied filter. Matching is case sensitive.
#'
#' Filter vectors require present, nonblank, unpadded values. Unknown values
#' error. Duplicate values normalize to their first occurrence before
#' filtering. Filtering subsets the stored component without changing its row
#' order.
#'
#' When no rows match, `get_summary()` retains the names, order, and storage
#' types documented for `report$summary`; `get_missing()` retains those
#' documented for `report$missing`. The `redcapmissing_labels` attribute
#' continues to contain labels for every event and instrument represented by
#' `report$plan`, including labels absent from the returned rows.
#'
#' @return For a comparison, the stratified tibble documented in
#'   [compare_reports()]. For a report, a tibble with exactly these columns
#'   and storage types:
#'
#' | Column | Storage and values |
#' |---|---|
#' | `redcap_event_name` | Character raw event name; `NA_character_` in classic projects |
#' | `instrument` | Character raw instrument name |
#' | `repeat_instrument` | Character raw repeating instrument; otherwise `NA_character_` |
#' | `repeat_instance` | Integer exact instance; otherwise `NA_integer_` |
#' | `validation_level` | Character: `"event:instrument"` or `"event:instrument:instance"` |
#' | `validation_check` | Character validation check code |
#' | `status` | Character: `"assessed"` or `"not applicable"` |
#' | `reason` | Character; typed missing unless the check is not applicable |
#' | `assessed`, `passed`, `failed` | Integer counts |
#' | `pass_rate`, `fail_rate` | Double proportions; `NA_real_` when nothing was assessed |
#'
#' The tibble has a `redcapmissing_labels` attribute containing named character
#' vectors `events` and `instruments` for presentation. Raw values remain in
#' the returned data and are used for filtering.
#'
#' @examples
#' \dontrun{
#' # report and instruments are caller supplied.
#' get_summary(report)
#' get_summary(
#'   report,
#'   validation_check = "field-complete",
#'   instruments = instruments
#' )
#' }
#'
#' @seealso [get_missing()], [run_plan()], [registry()], [flexify()]
#'
#' @export
get_summary <- function(
  report,
  validation_check = NULL,
  events = NULL,
  instruments = NULL,
  ...
) {
  UseMethod("get_summary")
}

#' @rdname get_summary
#' @export
get_summary.redcapmissing <- function(
  report,
  validation_check = NULL,
  events = NULL,
  instruments = NULL,
  ...
) {
  if (length(list(...))) .condition_signal_error("Unused arguments in `...`.")
  .report_validate_object(report, arg = "report")
  .summary_validate_rows(report$summary)

  filters <- .report_resolve_filters(
    report = report,
    validation_check = validation_check,
    events = events,
    instruments = instruments
  )
  summary_rows <- .report_filter_rows(
    rows = report$summary,
    filters = filters
  )
  out <- tibble::as_tibble(summary_rows)

  .report_attach_labels(out, report)
}

#' @rdname get_summary
#' @export
get_summary.default <- function(report, validation_check = NULL, events = NULL,
                                instruments = NULL, ...) {
  .condition_signal_error(
    "`report` must be a `redcapmissing` report or `redcapmissing_comparison`."
  )
}

#' Get unresolved missing rows from a REDCap missingness report
#'
#' `get_missing()` returns effective unresolved failures stored by [run_plan()].
#' Rows successfully overridden by verification are omitted. Filters select rows
#' from the stored missing component. Assessment results and denominators remain
#' those computed by [run_plan()].
#'
#' @inheritParams get_summary
#' @param report A `redcapmissing` report created by [run_plan()].
#'
#' @return A tibble with exactly these columns and storage types:
#'
#' | Column | Storage and meaning |
#' |---|---|
#' | `record_id` | Character normalized record ID |
#' | `redcap_event_name` | Character raw event name; `NA_character_` in classic projects |
#' | `repeat_instrument` | Character raw repeating instrument; otherwise `NA_character_` |
#' | `repeat_instance` | Integer exact instance; otherwise `NA_integer_` |
#' | `validation_context` | Character display context for the event/repeat location |
#' | `instrument` | Character raw instrument name |
#' | `validation_check` | Character validation check code from [registry()] |
#' | `field_name`, `field_label`, `field_type`, `branching_logic` | Character field context; typed missing for target level check failures |
#' | `url` | Character REDCap data entry URL when it can be constructed; otherwise `NA_character_` |
#'
#' Structural absence is represented by typed missing values. The tibble has a
#' `redcapmissing_labels` attribute containing named character vectors `events`
#' and `instruments` for presentation; raw
#' values remain in the returned data and are used for filtering.
#'
#' @inheritSection get_summary Filter semantics
#'
#' @examples
#' \dontrun{
#' # report and instruments are caller supplied.
#' get_missing(report)
#' get_missing(report, validation_check = "field-complete")
#' get_missing(report, instruments = instruments)
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
  .report_validate_object(report, arg = "report")
  .missing_validate_rows(report$missing)

  filters <- .report_resolve_filters(
    report = report,
    validation_check = validation_check,
    events = events,
    instruments = instruments
  )
  missing_rows <- .report_filter_rows(
    rows = report$missing,
    filters = filters
  )
  out <- tibble::as_tibble(missing_rows)

  .report_attach_labels(out, report)
}

.report_validate_object <- function(x, arg = "x") {
  if (!inherits(x, "redcapmissing")) {
    .condition_signal_error(
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
  if (!is.list(x) || !(identical(names(x), expected_names) ||
                      identical(names(x), c(expected_names, "settings")))) {
    .condition_signal_error(
      paste0(
        "`", arg, "` must contain exactly: ",
        paste(expected_names, collapse = ", "), ", optionally followed by settings."
      ),
      "schema"
    )
  }
  .plan_validate_object(x$plan)
  invisible(x)
}

.summary_list_columns <- function() {
  c(
    "redcap_event_name",
    "instrument",
    "repeat_instrument",
    "repeat_instance",
    "validation_level",
    "validation_check",
    "status",
    "reason",
    "assessed",
    "passed",
    "failed",
    "pass_rate",
    "fail_rate"
  )
}

.summary_build_prototype <- function() {
  tibble::tibble(
    redcap_event_name = character(),
    instrument = character(),
    repeat_instrument = character(),
    repeat_instance = integer(),
    validation_level = character(),
    validation_check = character(),
    status = character(),
    reason = character(),
    assessed = integer(),
    passed = integer(),
    failed = integer(),
    pass_rate = numeric(),
    fail_rate = numeric()
  )
}

.summary_validate_rows <- function(summary_rows) {
  .report_validate_component_rows(
    rows = summary_rows,
    expected = .summary_build_prototype(),
    component = "summary"
  )
  invalid_checks <- setdiff(unique(summary_rows$validation_check), .registry_list_validation_checks())
  if (length(invalid_checks) > 0L) {
    stop("`report$summary` contains unknown validation check codes.", call. = FALSE)
  }
  invalid_status <- setdiff(unique(summary_rows$status), c("assessed", "not applicable"))
  if (length(invalid_status) > 0L) {
    stop("`report$summary$status` contains unsupported values.", call. = FALSE)
  }
  invisible(summary_rows)
}

.missing_list_columns <- function() {
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

.missing_build_prototype <- function() {
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

.missing_validate_rows <- function(missing_rows) {
  .report_validate_component_rows(
    rows = missing_rows,
    expected = .missing_build_prototype(),
    component = "missing"
  )
  invalid_checks <- setdiff(unique(missing_rows$validation_check), .registry_list_validation_checks())
  if (length(invalid_checks) > 0L) {
    stop("`report$missing` contains unknown validation check codes.", call. = FALSE)
  }
  invisible(missing_rows)
}

.report_validate_component_rows <- function(rows, expected, component) {
  expected_names <- names(expected)
  if (!is.data.frame(rows) || !identical(names(rows), expected_names)) {
    stop(
      "`report$", component, "` must use the current column names and order: ",
      paste(expected_names, collapse = ", "), ".",
      call. = FALSE
    )
  }

  actual_types <- vapply(rows, typeof, character(1))
  expected_types <- vapply(expected, typeof, character(1))
  if (!identical(actual_types, expected_types)) {
    mismatched <- names(expected_types)[actual_types != expected_types]
    stop(
      "`report$", component, "` must use the current column storage types. ",
      "Invalid column(s): ", paste(mismatched, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(rows)
}

.report_resolve_filters <- function(
  report,
  validation_check,
  events,
  instruments
) {
  list(
    validation_check = .report_resolve_filter(
      values = validation_check,
      arg = "validation_check",
      valid_values = .registry_list_validation_checks()
    ),
    events = .report_resolve_filter(
      values = events,
      arg = "events",
      valid_values = .report_list_scope_values(report, "events")
    ),
    instruments = .report_resolve_filter(
      values = instruments,
      arg = "instruments",
      valid_values = .report_list_scope_values(report, "instruments")
    )
  )
}

.report_resolve_filter <- function(values, arg, valid_values) {
  if (is.null(values)) {
    return(NULL)
  }
  if (!is.character(values) || length(values) == 0L) {
    stop("`", arg, "` must be `NULL` or a nonempty character vector.", call. = FALSE)
  }
  if (anyNA(values) || any(trimws(values) == "")) {
    stop("`", arg, "` may not contain `NA` or blank values.", call. = FALSE)
  }
  if (any(values != trimws(values))) {
    stop("`", arg, "` values may not contain surrounding whitespace.", call. = FALSE)
  }

  values <- unique(values)
  unknown_values <- setdiff(values, valid_values)
  if (length(unknown_values) > 0L) {
    stop(
      "Unknown `", arg, "` value(s): ",
      paste0("`", unknown_values, "`", collapse = ", "), ".",
      call. = FALSE
    )
  }

  values
}

.report_list_scope_values <- function(report, scope) {
  plan <- report$plan %||% list()
  targets <- plan$assessible_targets

  if (identical(scope, "instruments")) {
    values <- plan$instruments
    if (is.null(values)) {
      values <- report$summary$instrument
    }
  } else if (identical(scope, "events")) {
    values <- if (is.data.frame(targets) && "redcap_event_name" %in% names(targets)) {
      targets$redcap_event_name
    } else {
      report$summary$redcap_event_name
    }
  } else {
    stop("Unknown report scope `", scope, "`.", call. = FALSE)
  }

  if (!is.character(values)) {
    stop("The report plan contains invalid `", scope, "` values.", call. = FALSE)
  }
  unique(values[!is.na(values) & values != ""])
}

.report_filter_rows <- function(rows, filters) {
  keep <- rep(TRUE, nrow(rows))
  if (!is.null(filters$validation_check)) {
    keep <- keep & rows$validation_check %in% filters$validation_check
  }
  if (!is.null(filters$events)) {
    keep <- keep & rows$redcap_event_name %in% filters$events
  }
  if (!is.null(filters$instruments)) {
    keep <- keep & rows$instrument %in% filters$instruments
  }

  rows[keep, , drop = FALSE]
}

.report_attach_labels <- function(x, report) {
  project <- report$plan$project %||% list()
  events <- .report_list_scope_values(report, "events")
  if (is.character(project$event_labels) && !is.null(names(project$event_labels))) {
    labelled_events <- names(project$event_labels)
    events <- c(
      labelled_events[labelled_events %in% events],
      events[!events %in% labelled_events]
    )
  }
  instruments <- .report_list_scope_values(report, "instruments")

  attr(x, "redcapmissing_labels") <- list(
    events = .report_resolve_labels(project$event_labels, events),
    instruments = .report_resolve_labels(
      project$instrument_labels,
      instruments
    )
  )
  x
}

.report_resolve_labels <- function(labels, values) {
  fallback <- stats::setNames(values, values)
  if (!is.character(labels) || is.null(names(labels))) {
    return(fallback)
  }
  labels <- labels[!is.na(names(labels)) & names(labels) != ""]
  matched_values <- values[values %in% names(labels)]
  fallback[matched_values] <- unname(labels[matched_values])
  fallback
}
