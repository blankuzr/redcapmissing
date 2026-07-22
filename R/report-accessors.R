#' Get validation summaries from a REDCap missingness report
#'
#' @description
#' `get_summary()` returns the user-facing validation summary from a
#' [find_missing()] report. Each row represents one validation check and REDCap
#' context stored in the completed report.
#'
#' @details
#' Filters use raw, case-sensitive values and are combined by intersection.
#' They subset the completed report without rerunning validation or recalculating
#' denominators, and duplicate values are treated as a set. Event and form
#' values are validated against the configured report scope; validation checks
#' are validated against [registry()]. Valid filters with no matching rows
#' return a zero-row tibble with the documented schema.
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
#' @return An ordinary tibble with these columns:
#' \describe{
#'   \item{`redcap_event_name`}{The raw REDCap event name, or `""` when event
#'     context is not applicable.}
#'   \item{`form`}{The raw REDCap instrument/form name.}
#'   \item{`redcap_repeat_instrument`}{The raw REDCap repeat instrument, or
#'     `""` when repeat-instrument context is not applicable.}
#'   \item{`redcap_repeat_instance`}{The raw REDCap repeat instance, or `""`
#'     when repeat-instance context is not applicable.}
#'   \item{`validation_level`}{The validation context level.}
#'   \item{`validation_check`}{The canonical validation-check code.}
#'   \item{`assessed`}{The integer number of rows assessed.}
#'   \item{`passed`}{The integer number of rows that passed.}
#'   \item{`failed`}{The integer number of rows that failed.}
#'   \item{`pass_rate`}{The numeric pass fraction.}
#'   \item{`fail_rate`}{The numeric failure fraction.}
#' }
#'
#' The tibble carries a `redcapmissing_labels` attribute containing named
#' `events` and `forms` character vectors for presentation. Data manipulation
#' may drop this optional metadata; raw context values remain in the columns.
#'
#' @examples
#' \dontrun{
#' report <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = c("baseline_form", "followup_form")
#' )
#'
#' get_summary(report)
#' get_summary(
#'   report,
#'   validation_check = "field-complete",
#'   forms = "baseline_form"
#' )
#' }
#'
#' @seealso [get_missing()], [find_missing()], [registry()], [flexify()]
#'
#' @export
get_summary <- function(
  report,
  validation_check = NULL,
  events = NULL,
  forms = NULL
) {
  .redcapmissing_check_report(report, arg = "report")
  .redcapmissing_check_summary_rows(report$summary)

  filters <- .redcapmissing_resolve_accessor_filters(
    report = report,
    validation_check = validation_check,
    events = events,
    forms = forms
  )
  summary_rows <- .redcapmissing_filter_accessor_rows(
    rows = report$summary,
    filters = filters
  )
  out <- tibble::as_tibble(summary_rows[
    ,
    .redcapmissing_get_summary_columns(),
    drop = FALSE
  ])

  .redcapmissing_attach_labels(out, report)
}

# Internal schema helpers -------------------------------------------------

.redcapmissing_get_summary_columns <- function() {
  c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_level",
    "validation_check",
    "assessed",
    "passed",
    "failed",
    "pass_rate",
    "fail_rate"
  )
}

.redcapmissing_get_summary_prototype <- function() {
  expected <- .miss_empty_validation_summary()
  expected[, .redcapmissing_get_summary_columns(), drop = FALSE]
}

.redcapmissing_check_summary_rows <- function(summary_rows) {
  .redcapmissing_check_report_rows(
    rows = summary_rows,
    expected = .miss_empty_validation_summary(),
    component = "summary"
  )
}

.redcapmissing_check_report_rows <- function(rows, expected, component) {
  expected_names <- names(expected)
  component_label <- if (identical(component, "summary")) {
    "validation summary"
  } else {
    "missing-row"
  }
  if (!is.data.frame(rows) || !identical(names(rows), expected_names)) {
    stop(
      "`report$",
      component,
      "` must use the current ",
      component_label,
      " column names and order: ",
      paste(expected_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  column_types <- vapply(rows, typeof, character(1))
  expected_types <- vapply(expected, typeof, character(1))
  if (!identical(column_types, expected_types)) {
    mismatched <- names(expected_types)[column_types != expected_types]
    expected_description <- paste0(
      mismatched,
      " (`",
      expected_types[mismatched],
      "`)"
    )
    stop(
      "`report$",
      component,
      "` must use the current ",
      component_label,
      " column types. Expected ",
      paste(expected_description, collapse = ", "),
      ".",
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
  forms
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
    forms = .redcapmissing_resolve_accessor_filter(
      values = forms,
      arg = "forms",
      valid_values = .redcapmissing_report_scope_values(report, "forms")
    )
  )
}

.redcapmissing_resolve_accessor_filter <- function(values, arg, valid_values) {
  if (is.null(values)) {
    return(NULL)
  }
  if (!is.character(values) || length(values) == 0) {
    stop(
      "`",
      arg,
      "` must be `NULL` or a non-empty character vector.",
      call. = FALSE
    )
  }
  if (anyNA(values) || any(trimws(values) == "")) {
    stop(
      "`",
      arg,
      "` may not contain `NA` or blank values.",
      call. = FALSE
    )
  }

  values <- unique(values)
  unknown_values <- setdiff(values, valid_values)
  if (length(unknown_values) > 0) {
    stop(
      "Unknown `",
      arg,
      "` value(s): ",
      paste0("`", unknown_values, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  values
}

.redcapmissing_report_scope_values <- function(report, scope) {
  spec <- .redcapmissing_report_spec(report)
  if (identical(scope, "forms")) {
    values <- spec$forms
    if (is.null(values)) {
      values <- report$summary$form
    }
  } else if (identical(scope, "events")) {
    values <- spec$events
    if (is.null(values)) {
      event_labels <- spec$event_labels
      values <- if (!is.null(names(event_labels))) {
        names(event_labels)
      } else {
        report$summary$redcap_event_name
      }
    }
    if (is.list(values)) {
      values <- unlist(values, use.names = FALSE)
    }
  } else {
    stop("Unknown report scope `", scope, "`.", call. = FALSE)
  }

  if (is.null(values)) {
    values <- character()
  }
  if (!is.character(values)) {
    stop(
      "`report$spec$",
      scope,
      "` must contain raw character values.",
      call. = FALSE
    )
  }

  values <- values[!is.na(values) & trimws(values) != ""]
  unique(values)
}

.redcapmissing_filter_accessor_rows <- function(rows, filters) {
  keep <- rep(TRUE, nrow(rows))
  if (!is.null(filters$validation_check)) {
    keep <- keep & rows$validation_check %in% filters$validation_check
  }
  if (!is.null(filters$events)) {
    keep <- keep & rows$redcap_event_name %in% filters$events
  }
  if (!is.null(filters$forms)) {
    keep <- keep & rows$form %in% filters$forms
  }

  rows[keep, , drop = FALSE]
}

# Internal presentation metadata helpers ---------------------------------

.redcapmissing_attach_labels <- function(x, report) {
  spec <- .redcapmissing_report_spec(report)
  attr(x, "redcapmissing_labels") <- list(
    events = .redcapmissing_safe_labels(spec$event_labels),
    forms = .redcapmissing_safe_labels(spec$form_labels)
  )
  x
}

.redcapmissing_safe_labels <- function(labels) {
  empty <- structure(character(), names = character())
  if (is.null(labels)) {
    return(empty)
  }

  labels
}
