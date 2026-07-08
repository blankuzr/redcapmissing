#' Format a REDCap missingness report by event and form
#'
#' @description
#' `flex_event_forms()` formats a REDCap missingness report as a reduced
#' event/form `flextable`. It shows event row-started started/due counts,
#' form-level incomplete counts, and repeat-instance started/due counts when
#' repeat context is present.
#'
#' @details
#' The table is a reporting reduction of [tidy.redcapmissing()] plus report
#' metadata. The `N (started/due)` column shows `event-row-started` as
#' `started/due (%)`; non-longitudinal reports use a synthetic `Single event`
#' row with `Total N/Total N`. If multiple
#' `event-row-started` summary rows are present for the same event, they must
#' agree on `passed` and `assessed` counts. Form rows show failed
#' `form-started` plus failed `form-complete` counts as a percentage of the
#' assessed count for the event where the form is offered.
#'
#' `event-complete` rows are not shown. `form-started` is not shown as a
#' separate metric row, but failed `form-started` contexts contribute to
#' `Form Incomplete`.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Additional arguments passed to methods.
#'
#' @return A `flextable` object with event header rows and form rows nested
#'   under each event. Repeat instrument and instance columns are
#'   included only when the report contains repeat context. Repeat form rows
#'   show `instance-row-started` as `started/due (%)` in the N column;
#'   non-repeat form rows leave the N cell blank. This function requires the
#'   optional `flextable` and `glue` packages.
#'
#' @examples
#' \dontrun{
#' records <- redcapAPI::exportRecordsTyped(rcon)
#' report <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = c("status_form", "survey_form")
#' )
#'
#' event_form_table <- flex_event_forms(report)
#' flex_html(event_form_table)
#' }
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
  names(display_data)[names(display_data) == "N"] <- "N (started/due)"

  out <- flextable::flextable(display_data) |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all")

  header_rows <- which(flex_parts$row_type == "event")
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

  out <- .redcapmissing_flex_event_forms_empty(has_repeat)
  event_order <- .redcapmissing_flex_event_forms_event_order(contexts, x)
  for (event in event_order) {
    event_contexts <- contexts[
      contexts$redcap_event_name == event,
      ,
      drop = FALSE
    ]
    single_event <- .redcapmissing_flex_event_forms_is_single_event(event, contexts)
    event_stats <- .redcapmissing_flex_event_forms_event_stats(
      validation_set = validation_set,
      event = event,
      total_n = total_n,
      single_event = single_event
    )

    out <- rbind(
      out,
      .redcapmissing_flex_event_forms_event_row(
        event = event,
        event_stats = event_stats,
        x = x,
        has_repeat = has_repeat,
        single_event = single_event
      )
    )

    for (row in seq_len(nrow(event_contexts))) {
      context <- event_contexts[row, , drop = FALSE]
      out <- rbind(
        out,
        .redcapmissing_flex_event_forms_context_row(
          context = context,
          event_stats = event_stats,
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
  display_columns <- c(display_columns, "N", "Form Incomplete")

  list(
    data = out,
    row_type = out$row_type,
    display_columns = display_columns,
    total_n = total_n
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
  spec <- .redcapmissing_report_spec(x)
  event_values <- unlist(spec$events %||% list(), use.names = FALSE)
  if (length(event_values) == 0 && is.list(spec$project)) {
    event_values <- unlist(lapply(spec$project, function(project) {
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
  spec <- .redcapmissing_report_spec(x)
  form_values <- unique(.miss_chr_vec(spec$forms %||% character()))
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
  spec <- .redcapmissing_report_spec(x)
  spec$total_n %||% 0L
}

.redcapmissing_flex_event_forms_event_stats <- function(
  validation_set,
  event,
  total_n,
  single_event
) {
  if (isTRUE(single_event)) {
    return(.redcapmissing_flex_event_forms_stats(total_n, total_n))
  }

  event_rows <- .redcapmissing_flex_event_forms_event_summary_rows(
    validation_set = validation_set,
    event = event,
    validation_check = "event-row-started"
  )
  if (nrow(event_rows) > 0) {
    return(.redcapmissing_flex_event_forms_agreeing_event_stats(
      rows = event_rows,
      event = event
    ))
  }

  repeat_rows <- .redcapmissing_flex_event_forms_event_summary_rows(
    validation_set = validation_set,
    event = event,
    validation_check = "instance-row-started"
  )
  if (nrow(repeat_rows) > 0) {
    return(.redcapmissing_flex_event_forms_largest_stats(repeat_rows))
  }

  .redcapmissing_flex_event_forms_stats(0, 0)
}

.redcapmissing_flex_event_forms_event_summary_rows <- function(
  validation_set,
  event,
  validation_check
) {
  validation_set[
    validation_set$validation_check == validation_check &
      validation_set$redcap_event_name == event,
    ,
    drop = FALSE
  ]
}

.redcapmissing_flex_event_forms_agreeing_event_stats <- function(rows, event) {
  stats <- unique(rows[, c("passed", "assessed"), drop = FALSE])
  if (nrow(stats) == 0) {
    return(.redcapmissing_flex_event_forms_stats(0, 0))
  }

  if (nrow(stats) > 1) {
    stats_text <- paste0(stats$passed, "/", stats$assessed)
    stop(
      "`flex_event_forms()` found conflicting `event-row-started` summaries ",
      "for event `",
      event,
      "`: ",
      paste(stats_text, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  .redcapmissing_flex_event_forms_stats(stats$passed[[1]], stats$assessed[[1]])
}

.redcapmissing_flex_event_forms_largest_stats <- function(rows) {
  if (nrow(rows) == 0) {
    return(.redcapmissing_flex_event_forms_stats(0, 0))
  }

  rows <- rows[order(rows$assessed, rows$passed, decreasing = TRUE), , drop = FALSE]
  .redcapmissing_flex_event_forms_stats(rows$passed[[1]], rows$assessed[[1]])
}

.redcapmissing_flex_event_forms_stats <- function(passed, assessed) {
  list(
    passed = .redcapmissing_flex_event_forms_count_value(passed),
    assessed = .redcapmissing_flex_event_forms_count_value(assessed)
  )
}

.redcapmissing_flex_event_forms_count_value <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return(0)
  }

  x
}

.redcapmissing_flex_event_forms_format_stats <- function(stats) {
  pct <- .redcapmissing_flex_event_forms_pct(stats$passed, stats$assessed)
  as.character(glue::glue("{stats$passed}/{stats$assessed} ({pct}%)"))
}

.redcapmissing_flex_event_forms_empty <- function(has_repeat) {
  .redcapmissing_flex_event_forms_row(
    row_type = character(),
    event = character(),
    form = character(),
    repeat_instrument = character(),
    repeat_instance = character(),
    n = character(),
    form_incomplete = character(),
    has_repeat = has_repeat
  )
}

.redcapmissing_flex_event_forms_event_row <- function(
  event,
  event_stats,
  x,
  has_repeat,
  single_event
) {
  event_label <- if (isTRUE(single_event)) {
    "Single event"
  } else {
    .redcapmissing_flex_label_values(
      values = event,
      labels = (.redcapmissing_report_spec(x)$event_labels %||% character())
    )
  }

  .redcapmissing_flex_event_forms_row(
    row_type = "event",
    event = event_label,
    form = "",
    repeat_instrument = "",
    repeat_instance = "",
    n = .redcapmissing_flex_event_forms_format_stats(event_stats),
    form_incomplete = "",
    has_repeat = has_repeat
  )
}

.redcapmissing_flex_event_forms_context_row <- function(
  context,
  event_stats,
  validation_set,
  x,
  has_repeat
) {
  repeat_context <- .redcapmissing_flex_event_forms_is_repeat_context(context)
  row_stats <- if (repeat_context) {
    .redcapmissing_flex_event_forms_summary_stats(
      validation_set = validation_set,
      context = context,
      validation_check = "instance-row-started"
    )
  } else {
    event_stats
  }
  form_incomplete_denominator <- event_stats$assessed

  form_incomplete <- .redcapmissing_flex_event_forms_incomplete_count(
    validation_set = validation_set,
    context = context
  )

  .redcapmissing_flex_event_forms_row(
    row_type = "form",
    event = "",
    form = .redcapmissing_flex_label_values(
      values = context$form,
      labels = (.redcapmissing_report_spec(x)$form_labels %||% character())
    ),
    repeat_instrument = .redcapmissing_flex_label_values(
      values = context$redcap_repeat_instrument,
      labels = (.redcapmissing_report_spec(x)$form_labels %||% character())
    ),
    repeat_instance = context$redcap_repeat_instance,
    n = if (repeat_context) .redcapmissing_flex_event_forms_format_stats(row_stats) else "",
    form_incomplete = .redcapmissing_flex_event_forms_format_count(
      count = form_incomplete,
      denominator = form_incomplete_denominator
    ),
    has_repeat = has_repeat
  )
}

.redcapmissing_flex_event_forms_incomplete_count <- function(
  validation_set,
  context
) {
  form_started_failed <- .redcapmissing_flex_event_forms_summary_value(
    validation_set = validation_set,
    context = context,
    validation_check = "form-started",
    column = "failed"
  )
  form_complete_failed <- .redcapmissing_flex_event_forms_summary_value(
    validation_set = validation_set,
    context = context,
    validation_check = "form-complete",
    column = "failed"
  )

  form_started_failed + form_complete_failed
}

.redcapmissing_flex_event_forms_summary_stats <- function(
  validation_set,
  context,
  validation_check
) {
  summary_row <- .redcapmissing_flex_event_forms_summary_row(
    validation_set = validation_set,
    context = context,
    validation_check = validation_check
  )
  if (nrow(summary_row) == 0) {
    return(.redcapmissing_flex_event_forms_stats(0, 0))
  }

  .redcapmissing_flex_event_forms_stats(
    passed = summary_row$passed[[1]],
    assessed = summary_row$assessed[[1]]
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
  denominator
) {
  pct <- .redcapmissing_flex_event_forms_pct(count, denominator)
  as.character(glue::glue("{count} ({pct}%)"))
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
  form_incomplete,
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
  out[["Form Incomplete"]] <- form_incomplete
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
