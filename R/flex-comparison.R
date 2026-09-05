#' @rdname flex_event_instruments
#' @param population For comparisons, `"full"`, `"shared"`, or both (default).
#'   Each population has its own All/event/instrument hierarchy and paired
#'   Previous/Current metrics. Missing contexts display `"Out of scope"`.
#'   Empty shared populations display `"No shared targets"`. Rate differences
#'   are current minus previous in percentage points; undefined rates are blank.
#' @export
flex_event_instruments.redcapmissing_comparison <- function(
  x,
  missing_threshold = 0.10,
  population = c("full", "shared"),
  ...
) {
  if (length(list(...))) {
    .condition_signal_error("Unused arguments in `...`.")
  }
  .comparison_validate_object(x)
  .flex_event_instruments_validate_threshold(missing_threshold)
  population <- .comparison_resolve_population(population)
  .flex_require_packages(c("flextable", "glue"), "flex_event_instruments()")
  parts <- .comparison_build_instrument_table(x, missing_threshold, population)
  out <- flextable::flextable(parts$data) |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all")
  context_count <- ncol(parts$data) - 12L
  metric_columns <- names(parts$data)[seq.int(
    context_count + 1L,
    ncol(parts$data)
  )]
  out <- flextable::set_header_labels(
    out,
    values = stats::setNames(
      as.list(rep(c("Previous", "Current", "Change (pp)"), 4L)),
      metric_columns
    )
  )
  out <- flextable::add_header_row(
    out,
    values = c(
      "Context",
      "N (started/due)",
      "Instrument Incomplete",
      "Instrument Not Started",
      .flex_event_instruments_build_threshold_heading(missing_threshold)
    ),
    colwidths = c(context_count, rep(3L, 4L))
  )
  headings <- which(
    parts$row_type %in% c("population", "all", "event", "empty")
  )
  if (length(headings)) {
    out <- flextable::bold(out, i = headings, part = "body")
  }
  for (i in which(parts$row_type %in% c("population", "empty"))) {
    out <- flextable::merge_at(
      out,
      i = i,
      j = seq_len(ncol(parts$data)),
      part = "body"
    )
  }
  instruments <- which(parts$row_type == "instrument")
  if (length(instruments)) {
    out <- flextable::padding(
      out,
      i = instruments,
      j = "Instrument",
      padding.left = 16
    )
  }
  flextable::autofit(out) |>
    flextable::set_table_properties(opts_html = list(scroll = list()))
}

.comparison_build_instrument_table <- function(
  comparison,
  threshold,
  population
) {
  has_repeat <- any(!is.na(comparison$target_results$repeat_instance))
  identity <- c(
    "row_type",
    ".event",
    ".instrument",
    ".repeat_instrument",
    ".repeat_instance"
  )
  metrics <- c(
    "N (started/due)",
    "Instrument Incomplete",
    "Instrument Not Started",
    .flex_event_instruments_build_threshold_heading(threshold)
  )
  count_columns <- c(
    ".started_count",
    ".incomplete_count",
    ".not_started_count",
    ".threshold_count"
  )
  denominator_columns <- c(".started_denominator", rep(".denominator", 3L))
  result <- list()
  row_types <- character()
  for (selected in names(.comparison_population_labels())[
    names(.comparison_population_labels()) %in% population
  ]) {
    before_targets <- .comparison_extract_targets(
      comparison,
      "previous",
      selected
    )
    after_targets <- .comparison_extract_targets(
      comparison,
      "current",
      selected
    )
    before <- .flex_event_instruments_aggregate_targets(
      before_targets,
      comparison$plans$previous$project,
      threshold
    )$data
    after <- .flex_event_instruments_aggregate_targets(
      after_targets,
      comparison$plans$current$project,
      threshold
    )$data
    paired <- .comparison_pair_rows(before, after, identity)
    events <- unique(c(
      before$.event[before$row_type == "event"],
      after$.event[after$row_type == "event"]
    ))
    paired <- paired[
      order(
        ifelse(paired$row_type == "all", 0L, match(paired$.event, events)),
        match(paired$row_type, c("all", "event", "instrument")),
        seq_len(nrow(paired)),
        method = "radix"
      ),
    ]
    project <- comparison$plans$current$project
    display <- tibble::tibble(
      Event = ifelse(
        paired$row_type == "all",
        "All",
        ifelse(
          paired$row_type == "event",
          .flex_event_instruments_resolve_event_label(
            paired$.event,
            list(events = project$event_labels)
          ),
          ""
        )
      ),
      Instrument = .flex_apply_labels(
        paired$.instrument,
        project$instrument_labels
      )
    )
    if (has_repeat) {
      display[["Repeat Instrument"]] <- .flex_apply_labels(
        paired$.repeat_instrument,
        project$instrument_labels
      )
      display[["Repeat Instance"]] <- ifelse(
        is.na(paired$.repeat_instance),
        "",
        as.character(paired$.repeat_instance)
      )
    }
    for (j in seq_along(metrics)) {
      for (side in c("previous", "current")) {
        numerator <- paired[[paste0(side, "_", count_columns[[j]])]]
        denominator <- paired[[paste0(side, "_", denominator_columns[[j]])]]
        text <- rep("", nrow(paired))
        applicable <- !is.na(numerator) & !is.na(denominator)
        text[applicable] <- .flex_event_instruments_format_fraction(
          numerator[applicable],
          denominator[applicable]
        )
        absent <- !paired[[paste0(side, "_in_scope")]]
        metric_row <- if (j == 1L) {
          paired$row_type == "event" |
            (paired$row_type == "instrument" & !is.na(paired$.repeat_instance))
        } else {
          paired$row_type != "event"
        }
        text[absent & metric_row] <- "Out of scope"
        display[[paste(
          if (side == "previous") "Previous" else "Current",
          metrics[[j]]
        )]] <- text
      }
      before_rate <- paired[[paste0("previous_", count_columns[[j]])]] /
        paired[[paste0("previous_", denominator_columns[[j]])]]
      after_rate <- paired[[paste0("current_", count_columns[[j]])]] /
        paired[[paste0("current_", denominator_columns[[j]])]]
      delta <- after_rate - before_rate
      delta[!is.finite(delta)] <- NA_real_
      display[[paste(
        "Change",
        metrics[[j]],
        "(pp)"
      )]] <- .comparison_format_percentage_points(delta)
    }
    heading <- display[NA_integer_, ]
    for (name in names(heading)) {
      heading[[name]] <- ""
    }
    heading$Event <- .comparison_population_labels()[[selected]]
    if (selected == "shared" && !nrow(before_targets)) {
      display <- heading
      display$Event <- "No shared targets"
      types <- "empty"
    } else {
      types <- paired$row_type
    }
    result[[selected]] <- dplyr::bind_rows(heading, display)
    row_types <- c(row_types, "population", types)
  }
  list(data = dplyr::bind_rows(result), row_type = row_types)
}

.comparison_format_percentage_points <- function(x) {
  out <- rep("", length(x))
  present <- !is.na(x)
  out[present] <- paste0(
    formatC(x[present] * 100, digits = 1, format = "f"),
    " pp"
  )
  out
}
