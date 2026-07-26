#' Format a REDCap missingness report by event and instrument
#'
#' @description
#' `flex_event_instruments()` formats a [run_plan()] result as a reduced
#' event/instrument `flextable`. It reports started/due counts together with
#' instrument-level incomplete, not-started, and missing-threshold metrics.
#'
#' @details
#' Metrics are computed from `x$target_results`, which has one row per
#' Assessible target. Consequently, the table uses the frozen plan rather than
#' inferring opportunities from whichever rows happen to be present at run
#' time. Instrument metrics display `N/D (%)`; the `All` row sums instrument
#' opportunities without deduplicating records across instruments.
#'
#' `Instrument Incomplete` counts targets with a failed event-row,
#' repeat-instance-row, instrument-started, or field-complete check.
#' `Instrument Not Started` counts targets that failed before field-complete.
#' For the threshold metric, an upstream or instrument-started failure has a
#' missing fraction of one. A started instrument uses `fields_failed /
#' fields_assessed`; a not-applicable field-complete check has fraction zero.
#'
#' @param x A `redcapmissing` object created by [run_plan()].
#' @param missing_threshold A finite numeric scalar from zero through one.
#'   Fractions must be strictly greater than thresholds below one. At one,
#'   fractions equal to one are counted.
#' @param ... Additional arguments passed to methods; currently unused.
#'
#' @return A `flextable` containing an `All` row, event headers, and instrument
#'   rows. Repeat columns appear only when the plan contains repeat targets.
#'
#' @examples
#' \dontrun{
#' plan <- plan_from_data(records, rcon, c("status", "survey"))
#' report <- run_plan(plan, records, rcon)
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
  .redcapmissing_check_report(x)
  .redcapmissing_flex_event_instruments_check_threshold(missing_threshold)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex_event_instruments()")

  parts <- .redcapmissing_flex_event_instruments_build(
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

.redcapmissing_flex_event_instruments_build <- function(
  x,
  missing_threshold = 0.10
) {
  .redcapmissing_flex_event_instruments_check_threshold(missing_threshold)
  targets <- .redcapmissing_flex_event_instruments_targets(x)
  labels <- attr(get_summary(x), "redcapmissing_labels", exact = TRUE) %||% list()
  has_repeat <- nrow(targets) > 0L && (
    any(!is.na(targets$repeat_instrument)) ||
      any(!is.na(targets$repeat_instance))
  )

  contexts <- unique(targets[c(
    "redcap_event_name",
    "instrument",
    "repeat_instrument",
    "repeat_instance"
  )])
  event_values <- unique(targets$redcap_event_name)
  rows <- list()
  row_index <- 0L

  for (event in event_values) {
    event_targets <- targets[.redcapmissing_same_value(
      targets$redcap_event_name,
      event
    ), , drop = FALSE]
    event_stats <- .redcapmissing_flex_event_instruments_event_stats(event_targets)
    row_index <- row_index + 1L
    rows[[row_index]] <- .redcapmissing_flex_event_instruments_row(
      row_type = "event",
      event = .redcapmissing_flex_event_instruments_event_label(event, labels),
      instrument = "",
      repeat_instrument = "",
      repeat_instance = "",
      n = .redcapmissing_flex_event_instruments_format_stats(event_stats),
      incomplete = "",
      not_started = "",
      threshold = "",
      has_repeat = has_repeat
    )

    event_contexts <- contexts[.redcapmissing_same_value(
      contexts$redcap_event_name,
      event
    ), , drop = FALSE]
    for (context_index in seq_len(nrow(event_contexts))) {
      context <- event_contexts[context_index, , drop = FALSE]
      context_targets <- targets[
        .redcapmissing_flex_event_instruments_context_match(targets, context),
        ,
        drop = FALSE
      ]
      metrics <- .redcapmissing_flex_event_instruments_metrics(
        context_targets,
        missing_threshold
      )
      repeat_context <- !is.na(context$repeat_instance[[1]]) ||
        !is.na(context$repeat_instrument[[1]])
      repeat_stats <- list(
        passed = sum(context_targets$repeat_instance_row_started == "passed"),
        assessed = sum(context_targets$repeat_instance_row_started %in% c("passed", "failed"))
      )

      row_index <- row_index + 1L
      rows[[row_index]] <- .redcapmissing_flex_event_instruments_row(
        row_type = "instrument",
        event = "",
        instrument = .redcapmissing_flex_event_instruments_label(
          context$instrument[[1]],
          labels$instruments
        ),
        repeat_instrument = .redcapmissing_flex_event_instruments_label(
          context$repeat_instrument[[1]],
          labels$instruments
        ),
        repeat_instance = if (is.na(context$repeat_instance[[1]])) {
          ""
        } else {
          as.character(context$repeat_instance[[1]])
        },
        n = if (repeat_context) {
          .redcapmissing_flex_event_instruments_format_stats(repeat_stats)
        } else {
          ""
        },
        incomplete = .redcapmissing_flex_event_instruments_format_fraction(
          metrics$incomplete,
          metrics$denominator
        ),
        not_started = .redcapmissing_flex_event_instruments_format_fraction(
          metrics$not_started,
          metrics$denominator
        ),
        threshold = .redcapmissing_flex_event_instruments_format_fraction(
          metrics$threshold,
          metrics$denominator
        ),
        has_repeat = has_repeat,
        incomplete_count = metrics$incomplete,
        denominator = metrics$denominator,
        not_started_count = metrics$not_started,
        threshold_count = metrics$threshold
      )
    }
  }

  out <- if (length(rows) == 0L) {
    .redcapmissing_flex_event_instruments_empty(has_repeat)
  } else {
    dplyr::bind_rows(rows)
  }
  out <- .redcapmissing_flex_event_instruments_add_all(out, has_repeat)

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
      .redcapmissing_flex_event_instruments_threshold_heading(missing_threshold)
  )
}

.redcapmissing_flex_event_instruments_targets <- function(x) {
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
      "`x$target_results` must use the current target-result schema.",
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

.redcapmissing_flex_event_instruments_event_stats <- function(targets) {
  if (nrow(targets) == 0L) {
    return(list(passed = 0L, assessed = 0L))
  }
  keys <- paste(targets$record_id, .redcapmissing_key_value(targets$redcap_event_name), sep = "\r")
  split_rows <- split(seq_len(nrow(targets)), keys)
  statuses <- vapply(split_rows, function(index) {
    value <- unique(targets$event_row_started[index])
    applicable <- value[value %in% c("passed", "failed")]
    if (length(applicable) > 1L) {
      stop("Conflicting event-row-started results for one record-event context.", call. = FALSE)
    }
    if (length(applicable) == 0L) "not applicable" else applicable[[1]]
  }, character(1))
  if (all(statuses == "not applicable")) {
    return(list(passed = length(statuses), assessed = length(statuses)))
  }
  list(
    passed = sum(statuses == "passed"),
    assessed = sum(statuses %in% c("passed", "failed"))
  )
}

.redcapmissing_flex_event_instruments_metrics <- function(targets, threshold) {
  upstream_failed <- targets$event_row_started == "failed" |
    targets$repeat_instance_row_started == "failed" |
    targets$instrument_started == "failed"
  incomplete <- upstream_failed | targets$field_complete == "failed"

  fraction <- rep(0, nrow(targets))
  fraction[upstream_failed] <- 1
  started <- !upstream_failed & targets$instrument_started == "passed"
  with_fields <- started & targets$fields_assessed > 0L
  fraction[with_fields] <- targets$fields_failed[with_fields] /
    targets$fields_assessed[with_fields]
  threshold_hit <- if (identical(threshold, 1)) {
    fraction >= 1
  } else {
    fraction > threshold
  }

  list(
    denominator = nrow(targets),
    incomplete = sum(incomplete),
    not_started = sum(upstream_failed),
    threshold = sum(threshold_hit)
  )
}

.redcapmissing_flex_event_instruments_context_match <- function(x, context) {
  .redcapmissing_same_value(x$redcap_event_name, context$redcap_event_name[[1]]) &
    x$instrument == context$instrument[[1]] &
    .redcapmissing_same_value(x$repeat_instrument, context$repeat_instrument[[1]]) &
    .redcapmissing_same_value(x$repeat_instance, context$repeat_instance[[1]])
}

.redcapmissing_same_value <- function(x, value) {
  if (is.na(value)) is.na(x) else !is.na(x) & x == value
}

.redcapmissing_key_value <- function(x) {
  out <- as.character(x)
  out[is.na(x)] <- "<NA>"
  out
}

.redcapmissing_flex_event_instruments_event_label <- function(event, labels) {
  if (is.na(event)) {
    return("Single event")
  }
  .redcapmissing_flex_event_instruments_label(event, labels$events)
}

.redcapmissing_flex_event_instruments_label <- function(value, labels) {
  if (is.na(value)) {
    return("")
  }
  if (is.character(labels) && !is.null(names(labels)) && value %in% names(labels)) {
    return(unname(labels[[value]]))
  }
  value
}

.redcapmissing_flex_event_instruments_check_threshold <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0 || x > 1) {
    stop("`missing_threshold` must be one finite number from 0 through 1.", call. = FALSE)
  }
  invisible(x)
}

.redcapmissing_flex_event_instruments_threshold_heading <- function(x) {
  percent <- format(x * 100, scientific = FALSE, trim = TRUE, digits = 15)
  if (grepl(".", percent, fixed = TRUE)) {
    percent <- sub("0+$", "", percent)
    percent <- sub("[.]$", "", percent)
  }
  if (identical(x, 1)) {
    "Instrument = 100% Missing"
  } else {
    paste0("Instrument >", percent, "% Missing")
  }
}

.redcapmissing_flex_event_instruments_format_stats <- function(x) {
  .redcapmissing_flex_event_instruments_format_fraction(x$passed, x$assessed)
}

.redcapmissing_flex_event_instruments_format_fraction <- function(count, denominator) {
  percent <- if (denominator > 0L) round(count / denominator * 100, 1) else 0
  as.character(glue::glue("{count}/{denominator} ({percent}%)"))
}

.redcapmissing_flex_event_instruments_empty <- function(has_repeat) {
  .redcapmissing_flex_event_instruments_row(
    row_type = character(), event = character(), instrument = character(),
    repeat_instrument = character(), repeat_instance = character(), n = character(),
    incomplete = character(), not_started = character(), threshold = character(),
    has_repeat = has_repeat, incomplete_count = integer(), denominator = integer(),
    not_started_count = integer(), threshold_count = integer()
  )
}

.redcapmissing_flex_event_instruments_add_all <- function(x, has_repeat) {
  instrument_rows <- x$row_type == "instrument"
  denominator <- sum(x$.denominator[instrument_rows])
  all_row <- .redcapmissing_flex_event_instruments_row(
    row_type = "all", event = "All", instrument = "",
    repeat_instrument = "", repeat_instance = "", n = "",
    incomplete = .redcapmissing_flex_event_instruments_format_fraction(
      sum(x$.incomplete_count[instrument_rows]), denominator
    ),
    not_started = .redcapmissing_flex_event_instruments_format_fraction(
      sum(x$.not_started_count[instrument_rows]), denominator
    ),
    threshold = .redcapmissing_flex_event_instruments_format_fraction(
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

.redcapmissing_flex_event_instruments_row <- function(
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
  out
}