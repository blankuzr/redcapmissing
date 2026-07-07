#' Format a REDCap missingness report by event and form
#'
#' @description
#' `flex_event_forms()` formats a REDCap missingness report as a reduced
#' event/form `flextable`. It shows total record N, event row-started N, and
#' form-level form-complete and field-complete failure summaries.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Additional arguments passed to methods.
#'
#' @return A `flextable` object with one total row, event header rows, and form
#'   rows nested under each started event. Repeat instrument and instance
#'   columns are included only when the report contains repeat context. This
#'   function requires the optional `flextable` and `glue` packages.
#'
#' @seealso [find_missing()], [tidy.redcapmissing()], [flex()], [flex_html()]
#'
#' @export
flex_event_forms <- function(x, ...) {
  UseMethod("flex_event_forms")
}

#' @rdname flex_event_forms
#' @param ... Unused.
#' @export
flex_event_forms.redcapmissing <- function(x, ...) {
  .redcapmissing_check_report(x)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex_event_forms()")

  validation_set <- generics::tidy(x)
  flex_parts <- .redcapmissing_flex_event_forms_build(
    validation_set = validation_set,
    x = x
  )
  display_data <- flex_parts$data[, flex_parts$display_columns, drop = FALSE]

  out <- flextable::flextable(display_data) |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all")

  header_rows <- which(flex_parts$row_type %in% c("total", "event"))
  if (length(header_rows) > 0) {
    out <- flextable::bold(out, i = header_rows, bold = TRUE, part = "body")
  }

  form_rows <- which(flex_parts$row_type == "form")
  if (length(form_rows) > 0) {
    out <- flextable::padding(
      out,
      i = form_rows,
      j = "Form",
      padding.left = 16,
      part = "body"
    )
  }

  flextable::autofit(out)
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_flex_event_forms_build <- function(validation_set, x) {
  validation_set <- .redcapmissing_flex_event_forms_normalize(validation_set)
  total_n <- .redcapmissing_flex_event_forms_total_n(x)
  contexts <- .redcapmissing_flex_event_forms_contexts(validation_set, x)
  has_repeat <- .redcapmissing_flex_event_forms_has_repeat(contexts)

  out <- .redcapmissing_flex_event_forms_total_row(total_n, has_repeat)
  event_order <- .redcapmissing_flex_event_forms_event_order(contexts, x)
  for (event in event_order) {
    event_contexts <- contexts[
      contexts$redcap_event_name == event,
      ,
      drop = FALSE
    ]
    single_event <- .redcapmissing_flex_event_forms_is_single_event(event, contexts)
    event_n <- .redcapmissing_flex_event_forms_event_n(
      event = event,
      x = x,
      total_n = total_n,
      single_event = single_event
    )

    out <- rbind(
      out,
      .redcapmissing_flex_event_forms_event_row(
        event = event,
        event_n = event_n,
        x = x,
        has_repeat = has_repeat,
        single_event = single_event
      )
    )

    if (event_n == 0 && !single_event) {
      next
    }

    for (row in seq_len(nrow(event_contexts))) {
      context <- event_contexts[row, , drop = FALSE]
      out <- rbind(
        out,
        .redcapmissing_flex_event_forms_context_row(
          context = context,
          event_n = event_n,
          validation_set = validation_set,
          x = x,
          has_repeat = has_repeat
        )
      )
    }
  }

  display_columns <- c("Event", "Form")
  if (has_repeat) {
    display_columns <- c(display_columns, "Repeat Instrument", "Repeat Instance")
  }
  display_columns <- c(display_columns, "N", "Form Complete", "Field-Complete Fails")

  list(
    data = out,
    row_type = out$row_type,
    display_columns = display_columns
  )
}

.redcapmissing_flex_event_forms_normalize <- function(validation_set) {
  repeat_columns <- c("redcap_repeat_instrument", "redcap_repeat_instance")
  for (column in repeat_columns) {
    if (!column %in% names(validation_set)) {
      validation_set[[column]] <- ""
    }
  }

  character_columns <- c("redcap_event_name", "form", repeat_columns, "validation_check")
  for (column in character_columns) {
    validation_set[[column]] <- .miss_chr_vec(validation_set[[column]])
    validation_set[[column]][is.na(validation_set[[column]])] <- ""
  }

  validation_set
}

.redcapmissing_flex_event_forms_contexts <- function(validation_set, x) {
  context_checks <- c(
    "event-row-started",
    "instance-row-started",
    "form-started",
    "form-complete",
    "field-complete"
  )
  context_rows <- validation_set[
    validation_set$validation_check %in% context_checks &
      !.miss_is_blank_vec(validation_set$form),
    c(
      "redcap_event_name",
      "form",
      "redcap_repeat_instrument",
      "redcap_repeat_instance"
    ),
    drop = FALSE
  ]

  if (nrow(context_rows) == 0) {
    return(tibble::tibble(
      redcap_event_name = character(),
      form = character(),
      redcap_repeat_instrument = character(),
      redcap_repeat_instance = character()
    ))
  }

  context_rows$.context_order <- seq_len(nrow(context_rows))
  context_key <- .redcapmissing_flex_event_forms_key(context_rows)
  context_rows <- context_rows[!duplicated(context_key), , drop = FALSE]

  event_order <- .redcapmissing_flex_event_forms_project_events(x, context_rows)
  form_order <- .redcapmissing_flex_event_forms_project_forms(x, context_rows)
  context_rows$.event_order <- match(context_rows$redcap_event_name, event_order)
  context_rows$.form_order <- match(context_rows$form, form_order)
  context_rows$.event_order[is.na(context_rows$.event_order)] <- length(event_order) + 1
  context_rows$.form_order[is.na(context_rows$.form_order)] <- length(form_order) + 1

  context_rows <- context_rows[order(
    context_rows$.event_order,
    context_rows$.form_order,
    context_rows$.context_order
  ), , drop = FALSE]
  context_rows[, c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ), drop = FALSE]
}

.redcapmissing_flex_event_forms_project_events <- function(x, contexts) {
  event_values <- unlist(x$events %||% list(), use.names = FALSE)
  if (length(event_values) == 0 && is.list(x$project)) {
    event_values <- unlist(lapply(x$project, function(project) {
      project$events %||% project$form_events %||% character()
    }), use.names = FALSE)
  }
  event_values <- unique(.miss_chr_vec(event_values))
  event_values <- event_values[!.miss_is_blank_vec(event_values)]

  context_events <- unique(.miss_chr_vec(contexts$redcap_event_name))
  context_events <- context_events[!.miss_is_blank_vec(context_events)]
  event_values <- c(event_values, setdiff(context_events, event_values))
  if (length(event_values) == 0 && nrow(contexts) > 0) {
    event_values <- ""
  }

  event_values
}

.redcapmissing_flex_event_forms_project_forms <- function(x, contexts) {
  form_values <- unique(.miss_chr_vec(x$forms %||% character()))
  form_values <- form_values[!.miss_is_blank_vec(form_values)]
  context_forms <- unique(.miss_chr_vec(contexts$form))
  context_forms <- context_forms[!.miss_is_blank_vec(context_forms)]
  c(form_values, setdiff(context_forms, form_values))
}

.redcapmissing_flex_event_forms_event_order <- function(contexts, x) {
  project_events <- .redcapmissing_flex_event_forms_project_events(x, contexts)
  context_events <- unique(.miss_chr_vec(contexts$redcap_event_name))
  if (length(project_events) == 0) {
    return(context_events)
  }
  c(project_events, setdiff(context_events, project_events))
}

.redcapmissing_flex_event_forms_total_n <- function(x) {
  validation_rows <- x$validation_rows %||% tibble::tibble()
  id_col <- x$id_col %||% character()
  if (length(id_col) != 1 || !id_col %in% names(validation_rows)) {
    return(0L)
  }

  record_ids <- unique(.miss_chr_vec(validation_rows[[id_col]]))
  record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
  length(record_ids)
}

.redcapmissing_flex_event_forms_event_n <- function(
  event,
  x,
  total_n,
  single_event
) {
  if (isTRUE(single_event)) {
    return(total_n)
  }

  event_rows <- .redcapmissing_flex_event_forms_filter_raw_context(
    rows = x$event_row_started_checks %||% tibble::tibble(),
    event = event
  )
  if (nrow(event_rows) > 0) {
    return(.redcapmissing_flex_event_forms_count_passed_records(event_rows, x$id_col))
  }

  repeat_rows <- .redcapmissing_flex_event_forms_filter_raw_context(
    rows = x$instance_row_started_checks %||% tibble::tibble(),
    event = event
  )
  .redcapmissing_flex_event_forms_count_passed_records(repeat_rows, x$id_col)
}

.redcapmissing_flex_event_forms_filter_raw_context <- function(
  rows,
  event,
  form = NULL,
  repeat_instrument = NULL,
  repeat_instance = NULL
) {
  if (nrow(rows) == 0) {
    return(rows)
  }

  for (column in c("redcap_event_name", "form", "redcap_repeat_instrument", "redcap_repeat_instance")) {
    if (!column %in% names(rows)) {
      rows[[column]] <- ""
    }
    rows[[column]] <- .miss_chr_vec(rows[[column]])
    rows[[column]][is.na(rows[[column]])] <- ""
  }

  keep <- rows$redcap_event_name == event
  if (!is.null(form)) {
    keep <- keep & rows$form == form
  }
  if (!is.null(repeat_instrument)) {
    keep <- keep & rows$redcap_repeat_instrument == repeat_instrument
  }
  if (!is.null(repeat_instance)) {
    keep <- keep & rows$redcap_repeat_instance == repeat_instance
  }

  rows[keep, , drop = FALSE]
}

.redcapmissing_flex_event_forms_count_passed_records <- function(rows, id_col) {
  if (nrow(rows) == 0 || !id_col %in% names(rows)) {
    return(0L)
  }

  passed <- rows$validation_passed %in% TRUE
  record_ids <- unique(.miss_chr_vec(rows[[id_col]][passed]))
  record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
  length(record_ids)
}

.redcapmissing_flex_event_forms_total_row <- function(total_n, has_repeat) {
  .redcapmissing_flex_event_forms_row(
    row_type = "total",
    event = "Total N",
    form = "",
    repeat_instrument = "",
    repeat_instance = "",
    n = as.character(total_n),
    form_complete = "",
    field_fails = "",
    has_repeat = has_repeat
  )
}

.redcapmissing_flex_event_forms_event_row <- function(
  event,
  event_n,
  x,
  has_repeat,
  single_event
) {
  event_label <- if (isTRUE(single_event)) {
    "Single event"
  } else {
    .redcapmissing_flex_label_values(
      values = event,
      labels = x$event_labels %||% character()
    )
  }

  .redcapmissing_flex_event_forms_row(
    row_type = "event",
    event = event_label,
    form = "",
    repeat_instrument = "",
    repeat_instance = "",
    n = as.character(event_n),
    form_complete = "",
    field_fails = "",
    has_repeat = has_repeat
  )
}

.redcapmissing_flex_event_forms_context_row <- function(
  context,
  event_n,
  validation_set,
  x,
  has_repeat
) {
  repeat_context <- .redcapmissing_flex_event_forms_is_repeat_context(context)
  row_n <- if (repeat_context) {
    .redcapmissing_flex_event_forms_summary_value(
      validation_set = validation_set,
      context = context,
      validation_check = "instance-row-started",
      column = "passed"
    )
  } else {
    event_n
  }

  form_complete_passed <- .redcapmissing_flex_event_forms_summary_value(
    validation_set = validation_set,
    context = context,
    validation_check = "form-complete",
    column = "passed"
  )
  field_failed <- .redcapmissing_flex_event_forms_summary_value(
    validation_set = validation_set,
    context = context,
    validation_check = "field-complete",
    column = "failed"
  )
  field_assessed <- .redcapmissing_flex_event_forms_summary_value(
    validation_set = validation_set,
    context = context,
    validation_check = "field-complete",
    column = "assessed"
  )

  .redcapmissing_flex_event_forms_row(
    row_type = "form",
    event = "",
    form = .redcapmissing_flex_label_values(
      values = context$form,
      labels = x$form_labels %||% character()
    ),
    repeat_instrument = .redcapmissing_flex_label_values(
      values = context$redcap_repeat_instrument,
      labels = x$form_labels %||% character()
    ),
    repeat_instance = context$redcap_repeat_instance,
    n = if (repeat_context) as.character(row_n) else "",
    form_complete = .redcapmissing_flex_event_forms_format_count(
      count = form_complete_passed,
      denominator = row_n
    ),
    field_fails = .redcapmissing_flex_event_forms_format_count(
      count = field_failed,
      denominator = field_assessed,
      denominator_label = "fields"
    ),
    has_repeat = has_repeat
  )
}

.redcapmissing_flex_event_forms_summary_value <- function(
  validation_set,
  context,
  validation_check,
  column
) {
  summary_row <- .redcapmissing_flex_event_forms_summary_row(
    validation_set = validation_set,
    context = context,
    validation_check = validation_check
  )
  if (nrow(summary_row) == 0) {
    return(0)
  }

  value <- summary_row[[column]][[1]]
  if (length(value) != 1 || is.na(value)) {
    return(0)
  }
  value
}

.redcapmissing_flex_event_forms_summary_row <- function(
  validation_set,
  context,
  validation_check
) {
  rows <- validation_set[
    validation_set$validation_check == validation_check &
      validation_set$redcap_event_name == context$redcap_event_name &
      validation_set$form == context$form &
      validation_set$redcap_repeat_instrument == context$redcap_repeat_instrument &
      validation_set$redcap_repeat_instance == context$redcap_repeat_instance,
    ,
    drop = FALSE
  ]

  rows[seq_len(min(nrow(rows), 1)), , drop = FALSE]
}

.redcapmissing_flex_event_forms_format_count <- function(
  count,
  denominator,
  denominator_label = NULL
) {
  pct <- .redcapmissing_flex_event_forms_pct(count, denominator)
  if (is.null(denominator_label)) {
    return(as.character(glue::glue("{count} ({pct}%)")))
  }

  as.character(glue::glue(
    "{count} ({pct}% of {denominator} {denominator_label})"
  ))
}

.redcapmissing_flex_event_forms_pct <- function(count, denominator) {
  if (length(denominator) != 1 || is.na(denominator) || denominator <= 0) {
    return(0)
  }

  round((count / denominator) * 100, 1)
}

.redcapmissing_flex_event_forms_row <- function(
  row_type,
  event,
  form,
  repeat_instrument,
  repeat_instance,
  n,
  form_complete,
  field_fails,
  has_repeat
) {
  out <- tibble::tibble(
    row_type = row_type,
    Event = event,
    Form = form
  )
  if (has_repeat) {
    out[["Repeat Instrument"]] <- repeat_instrument
    out[["Repeat Instance"]] <- repeat_instance
  }
  out[["N"]] <- n
  out[["Form Complete"]] <- form_complete
  out[["Field-Complete Fails"]] <- field_fails
  out
}

.redcapmissing_flex_event_forms_key <- function(x) {
  paste(
    x$redcap_event_name,
    x$form,
    x$redcap_repeat_instrument,
    x$redcap_repeat_instance,
    sep = "\r"
  )
}

.redcapmissing_flex_event_forms_has_repeat <- function(contexts) {
  if (nrow(contexts) == 0) {
    return(FALSE)
  }

  any(
    !.miss_is_blank_vec(contexts$redcap_repeat_instrument) |
      !.miss_is_blank_vec(contexts$redcap_repeat_instance)
  )
}

.redcapmissing_flex_event_forms_is_repeat_context <- function(context) {
  !.miss_is_blank_vec(context$redcap_repeat_instrument) ||
    !.miss_is_blank_vec(context$redcap_repeat_instance)
}

.redcapmissing_flex_event_forms_is_single_event <- function(event, contexts) {
  .miss_is_blank_scalar(event) &&
    all(.miss_is_blank_vec(contexts$redcap_event_name))
}
