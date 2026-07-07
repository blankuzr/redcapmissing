#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the tidy validation summary from a REDCap missingness
#' report as a `flextable` for reporting workflows.
#'
#' @details
#' `flex()` is a display wrapper around [tidy.redcapmissing()]. It preserves
#' one row per validation summary context, applies REDCap event and form labels
#' for display, and formats passed/failed counts with percentages. The optional
#' `events`, `forms`, and `validation_check` arguments filter by raw values
#' present in `tidy(x)` before labels are applied.
#'
#' Use `flex_event_forms()` when you want the reduced event/form report with
#' form rows nested under event header rows instead of the full validation-check
#' summary.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Additional arguments passed to methods.
#'
#' @return A `flextable` object with labeled event/form context,
#'   human-readable validation check, and pass/fail counts for display. Repeat
#'   instrument and instance columns are included only when the displayed rows
#'   contain repeat context. This function requires the optional `flextable`
#'   and `glue` packages.
#'
#' @examplesIf requireNamespace("flextable", quietly = TRUE) && requireNamespace("glue", quietly = TRUE)
#' # After building a report with find_missing():
#' # flex(report)
#' # flex(report, validation_check = "field-complete")
#' # flex(report, events = "baseline_event", forms = "baseline_form")
#'
#' @seealso [find_missing()], [tidy.redcapmissing()], [flex_event_forms()],
#'   [flex_html()]
#'
#' @export
flex <- function(x, ...) {
  UseMethod("flex")
}

#' @rdname flex
#' @param events Optional character vector of raw REDCap `redcap_event_name`
#'   values. When supplied, `flex()` returns only matching event rows.
#' @param forms Optional character vector of raw REDCap form/instrument names.
#'   When supplied, `flex()` returns only matching form rows.
#' @param validation_check Optional character vector of raw validation-check
#'   values from [tidy.redcapmissing()], such as `"field-complete"`. When
#'   supplied, `flex()` returns only matching validation-check rows.
#' @param ... Unused.
#' @export
flex.redcapmissing <- function(
  x,
  events = NULL,
  forms = NULL,
  validation_check = NULL,
  ...
) {
  .redcapmissing_check_report(x)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex()")

  validation_set <- generics::tidy(x)
  validation_set <- .redcapmissing_flex_filter_validation_set(
    validation_set = validation_set,
    events = events,
    forms = forms,
    validation_check = validation_check
  )
  flex_data <- .redcapmissing_flex_format_validation_set(
    validation_set = validation_set,
    x = x
  )

  flex_data |>
    flextable::flextable() |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_flex_filter_validation_set <- function(
  validation_set,
  events,
  forms,
  validation_check
) {
  events <- .redcapmissing_flex_resolve_filter_values(
    values = events,
    arg = "events",
    value_label = "raw REDCap event name"
  )
  forms <- .redcapmissing_flex_resolve_filter_values(
    values = forms,
    arg = "forms",
    value_label = "raw REDCap form name"
  )
  validation_check <- .redcapmissing_flex_resolve_filter_values(
    values = validation_check,
    arg = "validation_check",
    value_label = "raw validation check value"
  )

  .redcapmissing_flex_check_filter_values(
    values = events,
    validation_set = validation_set,
    arg = "events",
    column = "redcap_event_name",
    unknown_label = "event"
  )
  .redcapmissing_flex_check_filter_values(
    values = forms,
    validation_set = validation_set,
    arg = "forms",
    column = "form",
    unknown_label = "form"
  )
  .redcapmissing_flex_check_filter_values(
    values = validation_check,
    validation_set = validation_set,
    arg = "validation_check",
    column = "validation_check",
    unknown_label = "validation check"
  )

  keep <- rep(TRUE, nrow(validation_set))
  if (!is.null(forms)) {
    keep <- keep & validation_set$form %in% forms
  }
  if (!is.null(events)) {
    keep <- keep & validation_set$redcap_event_name %in% events
  }
  if (!is.null(validation_check)) {
    keep <- keep & validation_set$validation_check %in% validation_check
  }

  out <- validation_set[keep, , drop = FALSE]
  active_filters <- .redcapmissing_flex_active_filter_args(
    events = events,
    forms = forms,
    validation_check = validation_check
  )
  if (length(active_filters) > 0 && nrow(out) == 0) {
    stop(
      "The supplied ",
      paste(active_filters, collapse = ", "),
      " filter(s) produced no validation rows.",
      call. = FALSE
    )
  }

  out
}

.redcapmissing_flex_resolve_filter_values <- function(
  values,
  arg,
  value_label
) {
  if (is.null(values)) {
    return(NULL)
  }
  if (!is.character(values)) {
    stop("`", arg, "` must be a character vector of ", value_label, "s.", call. = FALSE)
  }

  values <- unique(.miss_chr_vec(values))
  values <- values[!.miss_is_blank_vec(values)]
  if (length(values) == 0) {
    stop(
      "`",
      arg,
      "` must contain at least one non-blank ",
      value_label,
      ".",
      call. = FALSE
    )
  }

  values
}

.redcapmissing_flex_check_filter_values <- function(
  values,
  validation_set,
  arg,
  column,
  unknown_label
) {
  if (is.null(values)) {
    return(invisible(values))
  }

  valid_values <- unique(.miss_chr_vec(validation_set[[column]]))
  valid_values <- valid_values[!.miss_is_blank_vec(valid_values)]
  unknown_values <- setdiff(values, valid_values)
  if (length(unknown_values) > 0) {
    stop(
      "`",
      arg,
      "` must match ",
      unknown_label,
      "(s) in `x`. Unknown ",
      unknown_label,
      "(s): ",
      paste(unknown_values, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(values)
}

.redcapmissing_flex_active_filter_args <- function(
  events,
  forms,
  validation_check
) {
  c(
    if (!is.null(events)) "`events`",
    if (!is.null(forms)) "`forms`",
    if (!is.null(validation_check)) "`validation_check`"
  )
}

.redcapmissing_flex_format_validation_set <- function(validation_set, x) {
  event <- .redcapmissing_flex_label_values(
    values = validation_set$redcap_event_name,
    labels = x$event_labels %||% character()
  )
  form <- .redcapmissing_flex_label_values(
    values = validation_set$form,
    labels = x$form_labels %||% character()
  )
  passed <- as.character(glue::glue(
    "{validation_set$passed} ({round(validation_set$pass_rate * 100, 1)}%)"
  ))
  failed <- as.character(glue::glue(
    "{validation_set$failed} ({round(validation_set$fail_rate * 100, 1)}%)"
  ))

  out <- tibble::tibble(
    Event = event,
    Form = form
  )
  if (.redcapmissing_flex_has_repeat_context(validation_set)) {
    out[["Repeat Instrument"]] <- .redcapmissing_flex_label_values(
      values = validation_set$redcap_repeat_instrument,
      labels = x$form_labels %||% character()
    )
    out[["Repeat Instance"]] <- .miss_chr_vec(validation_set$redcap_repeat_instance)
  }

  out[["Validation Check"]] <- .redcapmissing_flex_labels(
    validation_set$validation_check
  )
  out[["Assessed"]] <- validation_set$assessed
  out[["Passed"]] <- passed
  out[["Failed"]] <- failed
  out
}

.redcapmissing_flex_label_values <- function(values, labels) {
  values <- .miss_chr_vec(values)
  labels <- labels %||% character()
  if (is.null(names(labels))) {
    names(labels) <- rep("", length(labels))
  }

  out <- unname(labels[values])
  use_raw <- is.na(out) | .miss_is_blank_vec(out)
  out[use_raw] <- values[use_raw]
  out[.miss_is_blank_vec(values)] <- ""
  out
}

.redcapmissing_flex_has_repeat_context <- function(validation_set) {
  repeat_columns <- c("redcap_repeat_instrument", "redcap_repeat_instance")
  if (!all(repeat_columns %in% names(validation_set))) {
    return(FALSE)
  }

  any(
    !.miss_is_blank_vec(validation_set$redcap_repeat_instrument) |
      !.miss_is_blank_vec(validation_set$redcap_repeat_instance)
  )
}
