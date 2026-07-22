#' Format a REDCap missingness report by event and form
#'
#' @description
#' `flex_event_forms()` formats a REDCap missingness report as a reduced
#' event/form `flextable`. It shows event and repeat-instance started/due
#' counts plus form-level incomplete, not-started, and configurable missingness
#' threshold metrics.
#'
#' @details
#' The table is a reporting reduction of [get_summary()] plus report
#' metadata. The `N (started/due)` column shows `event-row-started` as
#' `started/due (%)`; non-longitudinal reports use a synthetic `Single event`
#' row with `Total N/Total N`. If multiple
#' `event-row-started` summary rows are present for the same event, they must
#' agree on `passed` and `assessed` counts. The first body row summarizes all
#' displayed form opportunities by summing the shown form-row numerators and
#' denominators without deduplicating records across forms. In this reduced
#' table, `Form Incomplete` shows unique failed record contexts with
#' `event-row-started`,
#' `instance-row-started`, `form-started`, or `field-complete` failures for the
#' exact event/form/repeat context. Multiple missing fields in the same record
#' context count once. The form-row denominator is the exact row-started
#' assessed N for that same context: `event-row-started` for non-repeat
#' longitudinal rows and `instance-row-started` for repeat rows. Missing or
#' invalid exact row-started denominators are treated as broken report objects.
#' Non-longitudinal reports use `Total N` as the display-only row-started
#' denominator for the synthetic `Single event` display row. Because the
#' denominator follows exact `record_eligibility`, forms under the same event
#' can have different assessed Ns when `records`, event selection, or repeat
#' context gives them different eligible record contexts.
#'
#' `Form Not Started` uses that same denominator and counts a record context
#' once when an applicable `event-row-started`, `instance-row-started`, or
#' `form-started` check fails. The dynamically named threshold column treats a
#' start-check failure as an effective missing fraction of `1`. For a started
#' form it uses failed over assessed `field-complete` checks after record
#' eligibility, field filters, branching logic, and checkbox-root handling. A
#' started form with no applicable field checks has an effective fraction of
#' `0`. For cutoffs below
#' `1`, the comparison is strict and unrounded, so exactly 10% does not count
#' at the default cutoff. At a cutoff of `1`, the column changes to
#' `Form = 100% Missing` and counts contexts whose effective fraction is `1`.
#' Below `1`, the heading prints the cutoff percentage without unnecessary
#' trailing zeros; for example, `missing_threshold = 0.125` produces
#' `Form >12.5% Missing`.
#' `Form Not Started` and the threshold column display `N/D (%)` on form and
#' `All` rows; event-header cells are blank.
#'
#' Reports created before `redcapmissing` 5.2.0 must be regenerated with
#' [find_missing()], including reports that retain detailed validation rows.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param missing_threshold A finite numeric scalar from `0` through `1`;
#'   defaults to `0.10`. Below `1`, contexts whose unrounded effective missing
#'   fraction is strictly greater than the cutoff contribute to the threshold
#'   column, whose heading prints the cutoff percentage without unnecessary
#'   trailing zeros. At `1`, contexts whose effective fraction equals `1`
#'   contribute to `Form = 100% Missing`.
#' @param ... Additional arguments passed to methods; currently unused by the
#'   `redcapmissing` method.
#'
#' @return A `flextable` object with event header rows and form rows nested
#'   under each event. Repeat instrument and instance columns are
#'   included only when the report contains repeat context. Repeat form rows
#'   show `instance-row-started` as `started/due (%)` in the N column;
#'   non-repeat form rows leave the N cell blank. Form and `All` rows display
#'   `Form Not Started` and the dynamic missing-threshold metric as `N/D (%)`.
#'   This function requires the optional `flextable` and `glue` packages.
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
#' event_form_table_125 <- flex_event_forms(report, missing_threshold = 0.125)
#' flex_html(event_form_table)
#' }
#'
#' @seealso [find_missing()], [get_summary()], [flexify()], [flex_html()]
#'
#' @export
flex_event_forms <- function(x, missing_threshold = 0.10, ...) {
  UseMethod("flex_event_forms")
}

#' @rdname flex_event_forms
#' @export
flex_event_forms.redcapmissing <- function(
  x,
  missing_threshold = 0.10,
  ...
) {
  .redcapmissing_check_report(x)
  .redcapmissing_flex_event_forms_check_missing_threshold(missing_threshold)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex_event_forms()")

  validation_set <- get_summary(x)
  flex_parts <- .redcapmissing_flex_event_forms_build(
    validation_set = validation_set,
    x = x,
    missing_threshold = missing_threshold
  )
  display_data <- flex_parts$data[, flex_parts$display_columns, drop = FALSE]
  names(display_data)[names(display_data) == "N"] <- "N (started/due)"
  names(display_data)[names(display_data) == "Form Missing Threshold"] <-
    flex_parts$missing_threshold_heading

  out <- flextable::flextable(display_data) |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all")

  header_rows <- which(flex_parts$row_type %in% c("all", "event"))
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

.redcapmissing_flex_event_forms_build <- function(
  validation_set,
  x,
  missing_threshold = 0.10
) {
  .redcapmissing_flex_event_forms_check_missing_threshold(missing_threshold)
  validation_set <- .redcapmissing_flex_event_forms_normalize(validation_set)
  total_n <- .redcapmissing_flex_event_forms_total_n(x)
  contexts <- .redcapmissing_flex_event_forms_contexts(validation_set, x)
  has_repeat <- .redcapmissing_flex_event_forms_has_repeat(contexts)
  field_counts <- .redcapmissing_flex_event_forms_prepare_field_counts(
    x = x,
    contexts = contexts
  )
  classifications <- .redcapmissing_flex_event_forms_classify_contexts(
    field_counts = field_counts,
    x = x,
    missing_threshold = missing_threshold
  )

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
          classifications = classifications,
          has_repeat = has_repeat,
          single_event = single_event
        )
      )
    }
  }
  out <- .redcapmissing_flex_event_forms_add_all_row(out, has_repeat)

  display_columns <- c("Event", "Form")
  if (has_repeat) {
    display_columns <- c(display_columns, "Repeat Instrument", "Repeat Instance")
  }
  display_columns <- c(
    display_columns,
    "N",
    "Form Incomplete",
    "Form Not Started",
    "Form Missing Threshold"
  )

  list(
    data = out,
    row_type = out$row_type,
    display_columns = display_columns,
    total_n = total_n,
    missing_threshold_heading =
      .redcapmissing_flex_event_forms_threshold_heading(missing_threshold)
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

.redcapmissing_flex_event_forms_check_missing_threshold <- function(x) {
  valid <- is.numeric(x) &&
    !is.logical(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    x <= 1
  if (!isTRUE(valid)) {
    stop(
      "`missing_threshold` must be one finite numeric value from 0 through 1.",
      call. = FALSE
    )
  }

  invisible(x)
}

.redcapmissing_flex_event_forms_threshold_heading <- function(x) {
  if (x == 1) {
    return("Form = 100% Missing")
  }

  percent <- format(
    x * 100,
    scientific = FALSE,
    trim = TRUE,
    digits = 15
  )
  if (grepl(".", percent, fixed = TRUE)) {
    percent <- sub("0+$", "", percent)
    percent <- sub("[.]$", "", percent)
  }
  paste0("Form >", percent, "% Missing")
}

.redcapmissing_flex_event_forms_context_columns <- function() {
  c(
    "record_id",
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
}

.redcapmissing_flex_event_forms_prepare_field_counts <- function(x, contexts) {
  spec <- .redcapmissing_report_spec(x)
  field_counts <- spec$.flex_event_forms_field_counts
  if (is.null(field_counts)) {
    stop(
      "`flex_event_forms()` requires field-count data added by the current ",
      "`find_missing()`. Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }
  if (!is.data.frame(field_counts)) {
    stop(
      "`flex_event_forms()` found corrupt field-count data. Rerun ",
      "`find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  context_columns <- .redcapmissing_flex_event_forms_context_columns()
  required_columns <- c(
    context_columns,
    "field_assessed",
    "field_failed"
  )
  missing_columns <- setdiff(required_columns, names(field_counts))
  if (length(missing_columns) > 0) {
    stop(
      "`flex_event_forms()` field-count data is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  field_counts <- tibble::as_tibble(
    field_counts[, required_columns, drop = FALSE]
  )
  for (column in context_columns) {
    if (anyNA(field_counts[[column]])) {
      row <- which(is.na(field_counts[[column]]))[[1]]
      stop(
        "`flex_event_forms()` found a missing `",
        column,
        "` in cached row ",
        row,
        ". Rerun `find_missing()` to rebuild this report.",
        call. = FALSE
      )
    }
    field_counts[[column]] <- .miss_chr_vec(field_counts[[column]])
  }
  blank_record <- .miss_is_blank_vec(field_counts$record_id)
  if (any(blank_record)) {
    stop(
      "`flex_event_forms()` found a blank record ID in cached row ",
      which(blank_record)[[1]],
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  for (column in c("field_assessed", "field_failed")) {
    values <- field_counts[[column]]
    invalid <- !is.numeric(values) ||
      length(values) != nrow(field_counts)
    if (!isTRUE(invalid)) {
      invalid_rows <- which(
        is.na(values) |
          !is.finite(values) |
          values < 0 |
          values != floor(values)
      )
      invalid <- length(invalid_rows) > 0
    } else {
      invalid_rows <- seq_len(max(1L, nrow(field_counts)))
    }
    if (isTRUE(invalid)) {
      row <- invalid_rows[[1]]
      label <- if (nrow(field_counts) >= row) {
        .redcapmissing_flex_event_forms_record_context_label(
          field_counts[row, , drop = FALSE]
        )
      } else {
        paste0("cached row ", row)
      }
      stop(
        "`flex_event_forms()` found invalid `",
        column,
        "` for ",
        label,
        ". Counts must be finite, nonnegative whole numbers. Rerun ",
        "`find_missing()` to rebuild this report.",
        call. = FALSE
      )
    }
  }

  failed_exceeds_assessed <-
    field_counts$field_failed > field_counts$field_assessed
  if (any(failed_exceeds_assessed)) {
    row <- which(failed_exceeds_assessed)[[1]]
    stop(
      "`flex_event_forms()` found `field_failed` greater than ",
      "`field_assessed` for ",
      .redcapmissing_flex_event_forms_record_context_label(
        field_counts[row, , drop = FALSE]
      ),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  cache_keys <- .redcapmissing_flex_event_forms_record_key(field_counts)
  if (anyDuplicated(cache_keys)) {
    row <- which(duplicated(cache_keys))[[1]]
    stop(
      "`flex_event_forms()` found duplicate field-count data for ",
      .redcapmissing_flex_event_forms_record_context_label(
        field_counts[row, , drop = FALSE]
      ),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  eligibility <- spec$record_eligibility
  if (!is.data.frame(eligibility) ||
      any(!context_columns %in% names(eligibility))) {
    stop(
      "`flex_event_forms()` requires complete record-eligibility data. ",
      "Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }
  eligibility <- tibble::as_tibble(
    eligibility[, context_columns, drop = FALSE]
  )
  for (column in context_columns) {
    if (anyNA(eligibility[[column]])) {
      stop(
        "`flex_event_forms()` found incomplete record-eligibility data. ",
        "Rerun `find_missing()` to rebuild this report.",
        call. = FALSE
      )
    }
    eligibility[[column]] <- .miss_chr_vec(eligibility[[column]])
  }
  eligibility_keys <- .redcapmissing_flex_event_forms_record_key(eligibility)
  if (anyDuplicated(eligibility_keys)) {
    row <- which(duplicated(eligibility_keys))[[1]]
    stop(
      "`flex_event_forms()` found duplicate record eligibility for ",
      .redcapmissing_flex_event_forms_record_context_label(
        eligibility[row, , drop = FALSE]
      ),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  missing_cache_keys <- setdiff(eligibility_keys, cache_keys)
  if (length(missing_cache_keys) > 0) {
    row <- match(missing_cache_keys[[1]], eligibility_keys)
    stop(
      "`flex_event_forms()` has no field-count data for ",
      .redcapmissing_flex_event_forms_record_context_label(
        eligibility[row, , drop = FALSE]
      ),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }
  extra_cache_keys <- setdiff(cache_keys, eligibility_keys)
  if (length(extra_cache_keys) > 0) {
    row <- match(extra_cache_keys[[1]], cache_keys)
    stop(
      "`flex_event_forms()` found field-count data outside eligibility for ",
      .redcapmissing_flex_event_forms_record_context_label(
        field_counts[row, , drop = FALSE]
      ),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  displayed_keys <- unique(.redcapmissing_flex_event_forms_key(contexts))
  cache_context_keys <- unique(.redcapmissing_flex_event_forms_key(field_counts))
  missing_context_keys <- setdiff(displayed_keys, cache_context_keys)
  if (length(missing_context_keys) > 0) {
    row <- match(missing_context_keys[[1]],
      .redcapmissing_flex_event_forms_key(contexts)
    )
    stop(
      "`flex_event_forms()` has no cached records for ",
      .redcapmissing_flex_event_forms_context_label(
        contexts[row, , drop = FALSE]
      ),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }
  extra_context_keys <- setdiff(cache_context_keys, displayed_keys)
  if (length(extra_context_keys) > 0) {
    row <- match(extra_context_keys[[1]],
      .redcapmissing_flex_event_forms_key(field_counts)
    )
    stop(
      "`flex_event_forms()` found cached records for an undisplayed ",
      .redcapmissing_flex_event_forms_context_label(
        field_counts[row, , drop = FALSE]
      ),
      ". Rerun `find_missing()` to rebuild this report.",
      call. = FALSE
    )
  }

  field_counts
}

.redcapmissing_flex_event_forms_classify_contexts <- function(
  field_counts,
  x,
  missing_threshold
) {
  start_checks <- c(
    "event-row-started",
    "instance-row-started",
    "form-started"
  )
  missing_rows <- x$missing
  start_keys <- character()
  if (!is.null(missing_rows) && nrow(missing_rows) > 0) {
    missing_rows <- .redcapmissing_flex_event_forms_normalize(missing_rows)
    start_rows <- missing_rows[
      missing_rows$validation_check %in% start_checks,
      ,
      drop = FALSE
    ]
    if (nrow(start_rows) > 0) {
      start_keys <- unique(
        .redcapmissing_flex_event_forms_record_key(start_rows)
      )
    }
  }

  record_keys <- .redcapmissing_flex_event_forms_record_key(field_counts)
  field_counts$form_not_started <- record_keys %in% start_keys
  field_counts$effective_missing_fraction <- ifelse(
    field_counts$form_not_started,
    1,
    ifelse(
      field_counts$field_assessed == 0,
      0,
      field_counts$field_failed / field_counts$field_assessed
    )
  )
  field_counts$form_missing_threshold <- if (missing_threshold == 1) {
    field_counts$effective_missing_fraction == 1
  } else {
    field_counts$effective_missing_fraction > missing_threshold
  }
  field_counts
}

.redcapmissing_flex_event_forms_context_classifications <- function(
  classifications,
  context
) {
  classifications[
    classifications$redcap_event_name == context$redcap_event_name &
      classifications$form == context$form &
      classifications$redcap_repeat_instrument ==
        context$redcap_repeat_instrument &
      classifications$redcap_repeat_instance ==
        context$redcap_repeat_instance,
    ,
    drop = FALSE
  ]
}

.redcapmissing_flex_event_forms_contexts <- function(validation_set, x) {
  context_checks <- c(
    "event-row-started",
    "instance-row-started",
    "form-started",
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
    .redcapmissing_flex_event_forms_check_event_summary_conflicts(
      rows = event_rows,
      event = event
    )
    return(.redcapmissing_flex_event_forms_largest_stats(event_rows))
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

.redcapmissing_flex_event_forms_check_event_summary_conflicts <- function(rows, event) {
  if (nrow(rows) == 0) {
    return(invisible(rows))
  }

  context_key <- .redcapmissing_flex_event_forms_key(rows)
  unique_context_key <- unique(context_key)
  for (key in unique_context_key) {
    stats <- unique(rows[context_key == key, c("passed", "assessed"), drop = FALSE])
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
  }

  invisible(rows)
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
    form_not_started = character(),
    form_missing_threshold = character(),
    has_repeat = has_repeat,
    form_incomplete_count = numeric(),
    form_incomplete_denominator = numeric(),
    form_not_started_count = numeric(),
    form_not_started_denominator = numeric(),
    form_missing_threshold_count = numeric(),
    form_missing_threshold_denominator = numeric()
  )
}

.redcapmissing_flex_event_forms_add_all_row <- function(out, has_repeat) {
  form_rows <- out$row_type == "form"
  all_count <- sum(out$.form_incomplete_count[form_rows], na.rm = TRUE)
  all_denominator <- sum(out$.form_incomplete_denominator[form_rows], na.rm = TRUE)
  all_not_started_count <- sum(
    out$.form_not_started_count[form_rows],
    na.rm = TRUE
  )
  all_not_started_denominator <- sum(
    out$.form_not_started_denominator[form_rows],
    na.rm = TRUE
  )
  all_missing_threshold_count <- sum(
    out$.form_missing_threshold_count[form_rows],
    na.rm = TRUE
  )
  all_missing_threshold_denominator <- sum(
    out$.form_missing_threshold_denominator[form_rows],
    na.rm = TRUE
  )

  all_row <- .redcapmissing_flex_event_forms_row(
    row_type = "all",
    event = "All",
    form = "",
    repeat_instrument = "",
    repeat_instance = "",
    n = "",
    form_incomplete = .redcapmissing_flex_event_forms_format_fraction(
      count = all_count,
      denominator = all_denominator
    ),
    form_not_started = .redcapmissing_flex_event_forms_format_fraction(
      count = all_not_started_count,
      denominator = all_not_started_denominator
    ),
    form_missing_threshold = .redcapmissing_flex_event_forms_format_fraction(
      count = all_missing_threshold_count,
      denominator = all_missing_threshold_denominator
    ),
    has_repeat = has_repeat,
    form_incomplete_count = all_count,
    form_incomplete_denominator = all_denominator,
    form_not_started_count = all_not_started_count,
    form_not_started_denominator = all_not_started_denominator,
    form_missing_threshold_count = all_missing_threshold_count,
    form_missing_threshold_denominator = all_missing_threshold_denominator
  )

  rbind(all_row, out)
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
    form_not_started = "",
    form_missing_threshold = "",
    has_repeat = has_repeat
  )
}

.redcapmissing_flex_event_forms_context_row <- function(
  context,
  event_stats,
  validation_set,
  x,
  classifications,
  has_repeat,
  single_event
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
  form_incomplete_denominator <- .redcapmissing_flex_event_forms_denominator(
    validation_set = validation_set,
    context = context,
    x = x,
    repeat_context = repeat_context,
    single_event = single_event
  )

  form_incomplete <- .redcapmissing_flex_event_forms_incomplete_count(
    validation_set = validation_set,
    context = context,
    x = x
  )
  context_classifications <-
    .redcapmissing_flex_event_forms_context_classifications(
      classifications = classifications,
      context = context
    )
  if (nrow(context_classifications) != form_incomplete_denominator) {
    stop(
      "`flex_event_forms()` found ",
      nrow(context_classifications),
      " cached record context(s), but the displayed denominator is ",
      form_incomplete_denominator,
      " for ",
      .redcapmissing_flex_event_forms_context_label(context),
      ". Rerun `find_missing()` to rebuild the report.",
      call. = FALSE
    )
  }
  form_not_started <- sum(context_classifications$form_not_started)
  form_missing_threshold <- sum(
    context_classifications$form_missing_threshold
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
    form_not_started = .redcapmissing_flex_event_forms_format_fraction(
      count = form_not_started,
      denominator = form_incomplete_denominator
    ),
    form_missing_threshold = .redcapmissing_flex_event_forms_format_fraction(
      count = form_missing_threshold,
      denominator = form_incomplete_denominator
    ),
    has_repeat = has_repeat,
    form_incomplete_count = form_incomplete,
    form_incomplete_denominator = form_incomplete_denominator,
    form_not_started_count = form_not_started,
    form_not_started_denominator = form_incomplete_denominator,
    form_missing_threshold_count = form_missing_threshold,
    form_missing_threshold_denominator = form_incomplete_denominator
  )
}

.redcapmissing_flex_event_forms_denominator <- function(
  validation_set,
  context,
  x,
  repeat_context,
  single_event
) {
  if (isTRUE(single_event) && !isTRUE(repeat_context)) {
    return(.redcapmissing_flex_event_forms_single_event_denominator(x))
  }

  validation_check <- if (isTRUE(repeat_context)) {
    "instance-row-started"
  } else {
    "event-row-started"
  }
  summary_row <- .redcapmissing_flex_event_forms_required_summary_row(
    validation_set = validation_set,
    context = context,
    validation_check = validation_check
  )
  assessed <- summary_row$assessed[[1]]
  if (length(assessed) != 1 || is.na(assessed) || assessed <= 0) {
    stop(
      "`flex_event_forms()` found invalid `",
      validation_check,
      "` assessed N for ",
      .redcapmissing_flex_event_forms_context_label(context),
      ".",
      call. = FALSE
    )
  }

  assessed
}

.redcapmissing_flex_event_forms_single_event_denominator <- function(x) {
  total_n <- .redcapmissing_flex_event_forms_total_n(x)
  if (length(total_n) != 1 || is.na(total_n) || total_n <= 0) {
    stop(
      "`flex_event_forms()` requires a positive `x$spec$total_n` for ",
      "non-longitudinal `Single event` reports.",
      call. = FALSE
    )
  }

  total_n
}

.redcapmissing_flex_event_forms_incomplete_count <- function(
  validation_set,
  context,
  x
) {
  missing_rows <- x$missing
  if (is.null(missing_rows) || nrow(missing_rows) == 0) {
    return(0L)
  }

  missing_rows <- .redcapmissing_flex_event_forms_normalize(missing_rows)
  incomplete_checks <- c(
    "event-row-started",
    "instance-row-started",
    "form-started",
    "field-complete"
  )
  rows <- missing_rows[
    missing_rows$validation_check %in% incomplete_checks &
      missing_rows$redcap_event_name == context$redcap_event_name &
      missing_rows$form == context$form &
      missing_rows$redcap_repeat_instrument == context$redcap_repeat_instrument &
      missing_rows$redcap_repeat_instance == context$redcap_repeat_instance,
    ,
    drop = FALSE
  ]
  if (nrow(rows) == 0) {
    return(0L)
  }

  length(unique(.redcapmissing_flex_event_forms_record_key(rows)))
}

.redcapmissing_flex_event_forms_record_key <- function(rows) {
  paste(
    .miss_chr_vec(rows$record_id),
    .miss_chr_vec(rows$redcap_event_name),
    .miss_chr_vec(rows$form),
    .miss_chr_vec(rows$redcap_repeat_instrument),
    .miss_chr_vec(rows$redcap_repeat_instance),
    sep = "\r"
  )
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

.redcapmissing_flex_event_forms_required_summary_row <- function(
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
    stop(
      "`flex_event_forms()` could not find exact `",
      validation_check,
      "` summary for ",
      .redcapmissing_flex_event_forms_context_label(context),
      ".",
      call. = FALSE
    )
  }

  summary_row
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

.redcapmissing_flex_event_forms_format_fraction <- function(
  count,
  denominator
) {
  pct <- .redcapmissing_flex_event_forms_pct(count, denominator)
  as.character(glue::glue("{count}/{denominator} ({pct}%)"))
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
  form_not_started,
  form_missing_threshold,
  has_repeat,
  form_incomplete_count = NA_real_,
  form_incomplete_denominator = NA_real_,
  form_not_started_count = NA_real_,
  form_not_started_denominator = NA_real_,
  form_missing_threshold_count = NA_real_,
  form_missing_threshold_denominator = NA_real_
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
  out[["Form Not Started"]] <- form_not_started
  out[["Form Missing Threshold"]] <- form_missing_threshold
  out[[".form_incomplete_count"]] <- form_incomplete_count
  out[[".form_incomplete_denominator"]] <- form_incomplete_denominator
  out[[".form_not_started_count"]] <- form_not_started_count
  out[[".form_not_started_denominator"]] <- form_not_started_denominator
  out[[".form_missing_threshold_count"]] <- form_missing_threshold_count
  out[[".form_missing_threshold_denominator"]] <-
    form_missing_threshold_denominator
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

.redcapmissing_flex_event_forms_context_label <- function(context) {
  event <- context$redcap_event_name[[1]]
  form <- context$form[[1]]
  repeat_instrument <- context$redcap_repeat_instrument[[1]]
  repeat_instance <- context$redcap_repeat_instance[[1]]

  parts <- c(
    paste0("event `", event, "`"),
    paste0("form `", form, "`")
  )
  if (!.miss_is_blank_scalar(repeat_instrument)) {
    parts <- c(parts, paste0("repeat instrument `", repeat_instrument, "`"))
  }
  if (!.miss_is_blank_scalar(repeat_instance)) {
    parts <- c(parts, paste0("repeat instance `", repeat_instance, "`"))
  }

  paste(parts, collapse = ", ")
}

.redcapmissing_flex_event_forms_record_context_label <- function(context) {
  paste0(
    "record `",
    context$record_id[[1]],
    "`, ",
    .redcapmissing_flex_event_forms_context_label(context)
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
