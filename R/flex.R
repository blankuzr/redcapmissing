#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the tidy validation summary from a REDCap missingness
#' report as a `flextable` for reporting workflows.
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
#' @seealso [find_missing()], [tidy.redcapmissing()], [flex_html()]
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
#' @param ... Unused.
#' @export
flex.redcapmissing <- function(x, events = NULL, forms = NULL, ...) {
  .redcapmissing_check_report(x)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex()")

  validation_set <- generics::tidy(x)
  validation_set <- .redcapmissing_flex_filter_validation_set(
    validation_set = validation_set,
    x = x,
    events = events,
    forms = forms
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
  x,
  events,
  forms
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

  .redcapmissing_flex_check_forms(forms = forms, x = x)
  .redcapmissing_flex_check_events(events = events, validation_set = validation_set)

  keep <- rep(TRUE, nrow(validation_set))
  if (!is.null(forms)) {
    keep <- keep & validation_set$form %in% forms
  }
  if (!is.null(events)) {
    keep <- keep & validation_set$redcap_event_name %in% events
  }

  out <- validation_set[keep, , drop = FALSE]
  if ((!is.null(events) || !is.null(forms)) && nrow(out) == 0) {
    stop(
      "The supplied `events` and `forms` filters produced no validation rows.",
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

.redcapmissing_flex_check_forms <- function(forms, x) {
  if (is.null(forms)) {
    return(invisible(forms))
  }

  valid_forms <- unique(.miss_chr_vec(x$forms %||% character()))
  valid_forms <- valid_forms[!.miss_is_blank_vec(valid_forms)]
  unknown_forms <- setdiff(forms, valid_forms)
  if (length(unknown_forms) > 0) {
    stop(
      "`forms` must match form(s) in `x`. Unknown form(s): ",
      paste(unknown_forms, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(forms)
}

.redcapmissing_flex_check_events <- function(events, validation_set) {
  if (is.null(events)) {
    return(invisible(events))
  }

  valid_events <- unique(.miss_chr_vec(validation_set$redcap_event_name))
  valid_events <- valid_events[!.miss_is_blank_vec(valid_events)]
  unknown_events <- setdiff(events, valid_events)
  if (length(unknown_events) > 0) {
    stop(
      "`events` must match event(s) in `x`. Unknown event(s): ",
      paste(unknown_events, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(events)
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
