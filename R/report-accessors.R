#' Get validation summaries from a REDCap missingness report
#'
#' `get_summary()` returns the stored validation summary from a report created by
#' [run_plan()]. Filters select rows from the stored summary. Assessment results,
#' applicability, and denominators remain those computed by [run_plan()].
#'
#' @param report A validated `redcapmissing` object created by [run_plan()].
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
#' @details
#' Filter matching is case sensitive. Vectors require present, nonblank,
#' unpadded values. Unknown values error. Duplicate filter values normalize to
#' one value.
#'
#' @return A tibble with exactly these columns and storage types:
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
  instruments = NULL
) {
  .redcapmissing_check_report(report, arg = "report")
  .redcapmissing_check_summary_rows(report$summary)

  filters <- .redcapmissing_resolve_accessor_filters(
    report = report,
    validation_check = validation_check,
    events = events,
    instruments = instruments
  )
  summary_rows <- .redcapmissing_filter_accessor_rows(
    rows = report$summary,
    filters = filters
  )
  out <- tibble::as_tibble(summary_rows)

  .redcapmissing_attach_labels(out, report)
}

# Internal schema helpers -------------------------------------------------

.redcapmissing_get_summary_columns <- function() {
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

.redcapmissing_get_summary_prototype <- function() {
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

.redcapmissing_check_summary_rows <- function(summary_rows) {
  .redcapmissing_check_report_rows(
    rows = summary_rows,
    expected = .redcapmissing_get_summary_prototype(),
    component = "summary"
  )
  invalid_checks <- setdiff(unique(summary_rows$validation_check), .redcapmissing_validation_checks())
  if (length(invalid_checks) > 0L) {
    stop("`report$summary` contains unknown validation check codes.", call. = FALSE)
  }
  invalid_status <- setdiff(unique(summary_rows$status), c("assessed", "not applicable"))
  if (length(invalid_status) > 0L) {
    stop("`report$summary$status` contains unsupported values.", call. = FALSE)
  }
  invisible(summary_rows)
}

.redcapmissing_check_report_rows <- function(rows, expected, component) {
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

# Internal filter helpers -------------------------------------------------

.redcapmissing_resolve_accessor_filters <- function(
  report,
  validation_check,
  events,
  instruments
) {
  list(
    validation_check = .redcapmissing_resolve_accessor_filter(
      values = validation_check,
      arg = "validation_check",
      valid_values = .redcapmissing_validation_checks()
    ),
    events = .redcapmissing_resolve_accessor_filter(
      values = events,
      arg = "events",
      valid_values = .redcapmissing_report_scope_values(report, "events")
    ),
    instruments = .redcapmissing_resolve_accessor_filter(
      values = instruments,
      arg = "instruments",
      valid_values = .redcapmissing_report_scope_values(report, "instruments")
    )
  )
}

.redcapmissing_resolve_accessor_filter <- function(values, arg, valid_values) {
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

.redcapmissing_report_scope_values <- function(report, scope) {
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

.redcapmissing_filter_accessor_rows <- function(rows, filters) {
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

# Internal presentation metadata helpers ---------------------------------

.redcapmissing_attach_labels <- function(x, report) {
  project <- report$plan$project %||% list()
  events <- .redcapmissing_report_scope_values(report, "events")
  if (is.character(project$event_labels) && !is.null(names(project$event_labels))) {
    labelled_events <- names(project$event_labels)
    events <- c(
      labelled_events[labelled_events %in% events],
      events[!events %in% labelled_events]
    )
  }
  instruments <- .redcapmissing_report_scope_values(report, "instruments")

  attr(x, "redcapmissing_labels") <- list(
    events = .redcapmissing_resolve_labels(project$event_labels, events),
    instruments = .redcapmissing_resolve_labels(
      project$instrument_labels,
      instruments
    )
  )
  x
}

.redcapmissing_resolve_labels <- function(labels, values) {
  fallback <- stats::setNames(values, values)
  if (!is.character(labels) || is.null(names(labels))) {
    return(fallback)
  }
  labels <- labels[!is.na(names(labels)) & names(labels) != ""]
  matched_values <- values[values %in% names(labels)]
  fallback[matched_values] <- unname(labels[matched_values])
  fallback
}