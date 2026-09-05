#' Format a REDCap missingness report by event and instrument
#'
#' @description
#' `flex_event_instruments()` formats a [run_plan()] result as an event and
#' instrument `flextable`. It reports planned opportunities, started rows,
#' incomplete instruments, instruments that have not started, and missingness
#' above a selected threshold.
#'
#' @details
#' Metrics are computed from `x$target_results`, which has one row per
#' `assessible_targets` row. `x$plan$assessible_targets` defines every
#' denominator.
#' Instrument metrics display `N/D (%)`. Event header `N (started/due)` values
#' count unique record and event contexts: the numerator has a passed
#' `event-row-started` check and the denominator has an assessed event gate.
#' In a classic project the event gate is not applicable, so all planned record
#' contexts are shown as started and due.
#'
#' On a repeating instrument or event row, `N (started/due)` is the number of
#' passed `repeat-instance-row-started` checks over the number assessed. It is
#' blank for instrument rows whose target context stores `NA_integer_` in
#' `repeat_instance`. `Instrument Incomplete` counts targets with any failed
#' `event-row-started`, `repeat-instance-row-started`, `instrument-started`, or
#' `field-complete` check. `Instrument Not Started` counts targets with a
#' failed event, repeat instance, or instrument start check.
#'
#' For the threshold metric, an event, repeat instance, or instrument start
#' failure has a missing fraction of one. A started instrument uses
#' `fields_failed / fields_assessed`; a `field-complete` check with no
#' applicable fields has fraction zero. Fractions must exceed thresholds below
#' one; a threshold of one includes fractions equal to one. The `All` row sums
#' instrument opportunities and can count one record more than once when that
#' record has multiple planned instruments. Event header rows leave instrument
#' metrics blank.
#'
#' The optional packages `flextable` and `glue` are required when this function
#' is called. The error lists each missing package.
#'
#' @param x A `redcapmissing` object created by [run_plan()] or a
#'   `redcapmissing_comparison` created by [compare_reports()].
#' @param missing_threshold A finite numeric scalar from zero through one, with
#'   comparison behavior described in **Details**.
#' @param ... Additional arguments passed to methods. The comparison method
#'   accepts `population`.
#'
#' @return A `flextable` containing an `All` row, event headers, and instrument
#'   rows. Repeat columns appear only when the plan contains repeat targets.
#'   Comparisons contain a separate section for each selected population,
#'   with paired metrics calculated using the same `missing_threshold`.
#'
#' @examples
#' \dontrun{
#' # report is caller supplied.
#' flex_event_instruments(report)
#' }
#'
#' @seealso [run_plan()], [get_summary()], [flexify()], [flex_html()]
#'
#' @export
flex_event_instruments <- function(x, missing_threshold = 0.10, ...) {
  UseMethod("flex_event_instruments")
}

#' @rdname flex_event_instruments
#' @export
flex_event_instruments.redcapmissing <- function(
  x,
  missing_threshold = 0.10,
  ...
) {
  .report_validate_object(x)
  .flex_event_instruments_validate_threshold(missing_threshold)
  .flex_require_packages(c("flextable", "glue"), "flex_event_instruments()")

  parts <- .flex_event_instruments_build_table(
    x = x,
    missing_threshold = missing_threshold
  )
  display_data <- parts$data[, parts$display_columns, drop = FALSE]
  names(display_data)[names(display_data) == "N"] <- "N (started/due)"
  names(display_data)[names(display_data) == "Instrument Missing Threshold"] <-
    parts$missing_threshold_heading

  out <- flextable::flextable(display_data) |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all")

  heading_rows <- which(parts$row_type %in% c("all", "event"))
  if (length(heading_rows) > 0L) {
    out <- flextable::bold(out, i = heading_rows, bold = TRUE, part = "body")
  }
  instrument_rows <- which(parts$row_type == "instrument")
  if (length(instrument_rows) > 0L) {
    out <- flextable::padding(
      out,
      i = instrument_rows,
      j = "Instrument",
      padding.left = 16,
      part = "body"
    )
  }

  flextable::autofit(out)
}

# Internal helpers ---------------------------------------------------------

.flex_event_instruments_build_table <- function(
  x,
  missing_threshold = 0.10
) {
  .flex_event_instruments_validate_threshold(missing_threshold)
  targets <- .flex_event_instruments_build_targets(x)
  project <- x$plan$project %||% list()
  .flex_event_instruments_aggregate_targets(targets, project, missing_threshold)
}

#' Aggregate target outcomes for report and comparison presentations.
#' @param targets Validated target results, optionally restricted to shared keys.
#' @param project Project presentation metadata.
#' @param missing_threshold The common threshold used on both comparison sides.
#' @return Display rows plus raw identity and numeric numerator/denominator
#'   columns. Raw keys, never display labels, determine comparison alignment.
#' @noRd
.flex_event_instruments_aggregate_targets <- function(targets, project, missing_threshold) {
  event_values <- unique(targets$redcap_event_name)
  instrument_values <- unique(c(
    targets$instrument,
    targets$repeat_instrument[!is.na(targets$repeat_instrument)]
  ))
  labels <- list(
    events = .report_resolve_labels(
      project$event_labels,
      event_values[!is.na(event_values)]
    ),
    instruments = .report_resolve_labels(
      project$instrument_labels,
      instrument_values
    )
  )
  has_repeat <- nrow(targets) > 0L && (
    any(!is.na(targets$repeat_instrument)) ||
      any(!is.na(targets$repeat_instance))
  )

  context_columns <- c(
    "redcap_event_name",
    "instrument",
    "repeat_instrument",
    "repeat_instance"
  )
  contexts <- unique(targets[context_columns])

  if (!nrow(targets)) {
    out <- .flex_event_instruments_build_empty_table(has_repeat)
  } else {
    context_count <- nrow(contexts)
    context_groups <- .flex_event_instruments_build_group_id(
      dplyr::bind_rows(contexts, targets[context_columns])
    )
    context_id <- match(
      context_groups[context_count + seq_len(nrow(targets))],
      context_groups[seq_len(context_count)]
    )

    upstream_failed <- targets$event_row_started == "failed" |
      targets$repeat_instance_row_started == "failed" |
      targets$instrument_started == "failed"
    incomplete <- upstream_failed | targets$field_complete == "failed"
    missing_fraction <- numeric(nrow(targets))
    missing_fraction[upstream_failed] <- 1
    with_fields <- !upstream_failed &
      targets$instrument_started == "passed" &
      targets$fields_assessed > 0L
    missing_fraction[with_fields] <- targets$fields_failed[with_fields] /
      targets$fields_assessed[with_fields]
    threshold_hit <- if (missing_threshold == 1) {
      missing_fraction >= 1
    } else {
      missing_fraction > missing_threshold
    }

    context_denominator <- as.integer(tabulate(context_id, context_count))
    context_incomplete <- as.integer(tabulate(
      context_id[incomplete],
      context_count
    ))
    context_not_started <- as.integer(tabulate(
      context_id[upstream_failed],
      context_count
    ))
    context_threshold <- as.integer(tabulate(
      context_id[threshold_hit],
      context_count
    ))
    repeat_passed <- as.integer(tabulate(
      context_id[targets$repeat_instance_row_started == "passed"],
      context_count
    ))
    repeat_assessed <- as.integer(tabulate(
      context_id[
        targets$repeat_instance_row_started %in% c("passed", "failed")
      ],
      context_count
    ))
    repeat_context <- !is.na(contexts$repeat_instance) |
      !is.na(contexts$repeat_instrument)

    instrument_rows <- .flex_event_instruments_build_row(
      row_type = rep("instrument", context_count),
      event = rep("", context_count),
      instrument = .flex_event_instruments_resolve_label(
        contexts$instrument,
        labels$instruments
      ),
      repeat_instrument = .flex_event_instruments_resolve_label(
        contexts$repeat_instrument,
        labels$instruments
      ),
      repeat_instance = ifelse(
        is.na(contexts$repeat_instance),
        "",
        as.character(contexts$repeat_instance)
      ),
      n = ifelse(
        repeat_context,
        .flex_event_instruments_format_fraction(
          repeat_passed,
          repeat_assessed
        ),
        ""
      ),
      incomplete = .flex_event_instruments_format_fraction(
        context_incomplete,
        context_denominator
      ),
      not_started = .flex_event_instruments_format_fraction(
        context_not_started,
        context_denominator
      ),
      threshold = .flex_event_instruments_format_fraction(
        context_threshold,
        context_denominator
      ),
      has_repeat = has_repeat,
      incomplete_count = context_incomplete,
      denominator = context_denominator,
      not_started_count = context_not_started,
      threshold_count = context_threshold
    )
    instrument_rows$.event <- contexts$redcap_event_name
    instrument_rows$.instrument <- contexts$instrument
    instrument_rows$.repeat_instrument <- contexts$repeat_instrument
    instrument_rows$.repeat_instance <- contexts$repeat_instance
    instrument_rows$.started_count <- ifelse(repeat_context, repeat_passed, NA_integer_)
    instrument_rows$.started_denominator <- ifelse(repeat_context, repeat_assessed, NA_integer_)

    record_event_columns <- c("record_id", "redcap_event_name")
    record_events <- unique(targets[record_event_columns])
    record_event_count <- nrow(record_events)
    record_event_groups <- .flex_event_instruments_build_group_id(
      dplyr::bind_rows(record_events, targets[record_event_columns])
    )
    record_event_id <- match(
      record_event_groups[record_event_count + seq_len(nrow(targets))],
      record_event_groups[seq_len(record_event_count)]
    )
    record_event_passed <- as.integer(tabulate(
      record_event_id[targets$event_row_started == "passed"],
      record_event_count
    ))
    record_event_failed <- as.integer(tabulate(
      record_event_id[targets$event_row_started == "failed"],
      record_event_count
    ))
    if (any(record_event_passed > 0L & record_event_failed > 0L)) {
      stop(
        "Conflicting event-row-started results for one record and event context.",
        call. = FALSE
      )
    }
    record_event_status <- ifelse(
      record_event_passed > 0L,
      "passed",
      ifelse(record_event_failed > 0L, "failed", "not applicable")
    )
    record_event_event_id <- match(
      record_events$redcap_event_name,
      event_values
    )
    event_count <- length(event_values)
    event_record_count <- as.integer(tabulate(
      record_event_event_id,
      event_count
    ))
    event_passed <- as.integer(tabulate(
      record_event_event_id[record_event_status == "passed"],
      event_count
    ))
    event_assessed <- as.integer(tabulate(
      record_event_event_id[
        record_event_status %in% c("passed", "failed")
      ],
      event_count
    ))
    no_applicable_event_gate <- event_assessed == 0L
    event_passed[no_applicable_event_gate] <-
      event_record_count[no_applicable_event_gate]
    event_assessed[no_applicable_event_gate] <-
      event_record_count[no_applicable_event_gate]

    event_rows <- .flex_event_instruments_build_row(
      row_type = rep("event", event_count),
      event = .flex_event_instruments_resolve_event_label(
        event_values,
        labels
      ),
      instrument = rep("", event_count),
      repeat_instrument = rep("", event_count),
      repeat_instance = rep("", event_count),
      n = .flex_event_instruments_format_fraction(
        event_passed,
        event_assessed
      ),
      incomplete = rep("", event_count),
      not_started = rep("", event_count),
      threshold = rep("", event_count),
      has_repeat = has_repeat
    )
    event_rows$.event <- event_values
    event_rows$.started_count <- event_passed
    event_rows$.started_denominator <- event_assessed

    event_rows$.event_order <- seq_len(event_count)
    event_rows$.row_order <- 0L
    instrument_rows$.event_order <- match(
      contexts$redcap_event_name,
      event_values
    )
    instrument_rows$.row_order <- seq_len(context_count)
    out <- dplyr::bind_rows(event_rows, instrument_rows)
    out <- out[order(out$.event_order, out$.row_order), , drop = FALSE]
    out$.event_order <- NULL
    out$.row_order <- NULL
  }

  out <- .flex_event_instruments_add_all_rows(out, has_repeat)

  display_columns <- c("Event", "Instrument")
  if (has_repeat) {
    display_columns <- c(display_columns, "Repeat Instrument", "Repeat Instance")
  }
  display_columns <- c(
    display_columns,
    "N",
    "Instrument Incomplete",
    "Instrument Not Started",
    "Instrument Missing Threshold"
  )

  list(
    data = out,
    row_type = out$row_type,
    display_columns = display_columns,
    missing_threshold_heading =
      .flex_event_instruments_build_threshold_heading(missing_threshold)
  )
}

.flex_event_instruments_build_targets <- function(x) {
  required <- c(
    "record_id", "instrument", "redcap_event_name", "repeat_instrument",
    "repeat_instance", "target_source", "event_row_started",
    "repeat_instance_row_started", "instrument_started", "field_complete",
    "fields_assessed", "fields_failed", "field_applicability_reason"
  )
  expected_types <- c(
    record_id = "character",
    instrument = "character",
    redcap_event_name = "character",
    repeat_instrument = "character",
    repeat_instance = "integer",
    target_source = "character",
    event_row_started = "character",
    repeat_instance_row_started = "character",
    instrument_started = "character",
    field_complete = "character",
    fields_assessed = "integer",
    fields_failed = "integer",
    field_applicability_reason = "character"
  )
  targets <- x$target_results
  if (!is.data.frame(targets) || !identical(names(targets), required)) {
    stop(
      "`x$target_results` must use the current target result schema.",
      call. = FALSE
    )
  }
  actual_types <- vapply(targets, typeof, character(1))
  if (!identical(actual_types, expected_types)) {
    stop("`x$target_results` has invalid column storage types.", call. = FALSE)
  }
  valid_status <- c("passed", "failed", "not applicable", "not reached")
  status_columns <- c(
    "event_row_started", "repeat_instance_row_started",
    "instrument_started", "field_complete"
  )
  invalid <- unlist(lapply(targets[status_columns], setdiff, y = valid_status))
  if (length(invalid) > 0L) {
    stop("`x$target_results` contains unsupported check statuses.", call. = FALSE)
  }
  targets
}

.flex_event_instruments_build_group_id <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  row_count <- nrow(x)
  if (!row_count) return(integer())
  ordering <- do.call(
    order,
    c(unname(x), list(na.last = TRUE, method = "radix"))
  )
  if (row_count == 1L) return(1L)

  same_as_previous <- rep(TRUE, row_count - 1L)
  for (column in x) {
    sorted <- column[ordering]
    previous <- sorted[-row_count]
    following <- sorted[-1L]
    equal <- (is.na(previous) & is.na(following)) |
      (!is.na(previous) & !is.na(following) & previous == following)
    same_as_previous <- same_as_previous & equal
  }
  sorted_id <- cumsum(c(TRUE, !same_as_previous))
  group_id <- integer(row_count)
  group_id[ordering] <- sorted_id
  group_id
}

.flex_event_instruments_resolve_event_label <- function(event, labels) {
  out <- .flex_event_instruments_resolve_label(event, labels$events)
  out[is.na(event)] <- "Single event"
  out
}

.flex_event_instruments_resolve_label <- function(value, labels) {
  .flex_apply_labels(value, labels)
}

.flex_event_instruments_validate_threshold <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0 || x > 1) {
    stop("`missing_threshold` must be one finite number from 0 through 1.", call. = FALSE)
  }
  invisible(x)
}

.flex_event_instruments_build_threshold_heading <- function(x) {
  percent <- format(x * 100, scientific = FALSE, trim = TRUE, digits = 15)
  if (grepl(".", percent, fixed = TRUE)) {
    percent <- sub("0+$", "", percent)
    percent <- sub("[.]$", "", percent)
  }
  if (x == 1) {
    "Instrument = 100% Missing"
  } else {
    paste0("Instrument >", percent, "% Missing")
  }
}

.flex_event_instruments_format_statistics <- function(x) {
  .flex_event_instruments_format_fraction(x$passed, x$assessed)
}

.flex_event_instruments_format_fraction <- function(count, denominator) {
  count <- as.integer(count)
  denominator <- as.integer(denominator)
  percent <- numeric(length(denominator))
  assessed <- denominator > 0L
  percent[assessed] <- round(count[assessed] / denominator[assessed] * 100, 1)
  paste0(count, "/", denominator, " (", percent, "%)")
}

.flex_event_instruments_build_empty_table <- function(has_repeat) {
  .flex_event_instruments_build_row(
    row_type = character(), event = character(), instrument = character(),
    repeat_instrument = character(), repeat_instance = character(), n = character(),
    incomplete = character(), not_started = character(), threshold = character(),
    has_repeat = has_repeat, incomplete_count = integer(), denominator = integer(),
    not_started_count = integer(), threshold_count = integer()
  )
}

.flex_event_instruments_add_all_rows <- function(x, has_repeat) {
  instrument_rows <- x$row_type == "instrument"
  denominator <- sum(x$.denominator[instrument_rows])
  all_row <- .flex_event_instruments_build_row(
    row_type = "all", event = "All", instrument = "",
    repeat_instrument = "", repeat_instance = "", n = "",
    incomplete = .flex_event_instruments_format_fraction(
      sum(x$.incomplete_count[instrument_rows]), denominator
    ),
    not_started = .flex_event_instruments_format_fraction(
      sum(x$.not_started_count[instrument_rows]), denominator
    ),
    threshold = .flex_event_instruments_format_fraction(
      sum(x$.threshold_count[instrument_rows]), denominator
    ),
    has_repeat = has_repeat,
    incomplete_count = sum(x$.incomplete_count[instrument_rows]),
    denominator = denominator,
    not_started_count = sum(x$.not_started_count[instrument_rows]),
    threshold_count = sum(x$.threshold_count[instrument_rows])
  )
  dplyr::bind_rows(all_row, x)
}

.flex_event_instruments_build_row <- function(
  row_type,
  event,
  instrument,
  repeat_instrument,
  repeat_instance,
  n,
  incomplete,
  not_started,
  threshold,
  has_repeat,
  incomplete_count = NA_integer_,
  denominator = NA_integer_,
  not_started_count = NA_integer_,
  threshold_count = NA_integer_
) {
  out <- tibble::tibble(
    row_type = row_type,
    Event = event,
    Instrument = instrument
  )
  if (has_repeat) {
    out[["Repeat Instrument"]] <- repeat_instrument
    out[["Repeat Instance"]] <- repeat_instance
  }
  out[["N"]] <- n
  out[["Instrument Incomplete"]] <- incomplete
  out[["Instrument Not Started"]] <- not_started
  out[["Instrument Missing Threshold"]] <- threshold
  out[[".incomplete_count"]] <- as.integer(incomplete_count)
  out[[".denominator"]] <- as.integer(denominator)
  out[[".not_started_count"]] <- as.integer(not_started_count)
  out[[".threshold_count"]] <- as.integer(threshold_count)
  out[[".event"]] <- rep(NA_character_, nrow(out))
  out[[".instrument"]] <- rep(NA_character_, nrow(out))
  out[[".repeat_instrument"]] <- rep(NA_character_, nrow(out))
  out[[".repeat_instance"]] <- rep(NA_integer_, nrow(out))
  out[[".started_count"]] <- rep(NA_integer_, nrow(out))
  out[[".started_denominator"]] <- rep(NA_integer_, nrow(out))
  out
}
