#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools to run this benchmark script.", call. = FALSE)
}

devtools::load_all(quiet = TRUE)
source(file.path("tests", "testthat", "helper-fixtures.R"), local = TRUE)

bench_iterations <- as.integer(Sys.getenv("REDCAPMISSING_BENCH_ITERATIONS", "3"))
if (is.na(bench_iterations) || bench_iterations < 1L) {
  bench_iterations <- 3L
}

requested_tiers <- strsplit(
  Sys.getenv("REDCAPMISSING_BENCH_TIERS", "small,medium"),
  ",",
  fixed = TRUE
)[[1]]
requested_tiers <- trimws(requested_tiers)
requested_tiers <- requested_tiers[nzchar(requested_tiers)]

make_form_names <- function(n_forms) {
  sprintf("form_%02d", seq_len(n_forms))
}

make_field_names <- function(form, n_fields) {
  sprintf("%s_field_%03d", form, seq_len(n_fields))
}

make_benchmark_meta <- function(forms, fields_per_form, longitudinal_branches = FALSE) {
  rows <- list(meta_row("record_id", forms[[1]], field_label = "Record ID", required = "y"))
  row_i <- 2L

  for (form in forms) {
    fields <- make_field_names(form, fields_per_form)
    for (field_i in seq_along(fields)) {
      branching <- ""
      if (field_i > 1L && field_i %% 10L == 0L) {
        prior_field <- fields[[field_i - 1L]]
        branching <- if (isTRUE(longitudinal_branches)) {
          sprintf("[event_01][%s] <> ''", prior_field)
        } else {
          sprintf("[%s] <> ''", prior_field)
        }
      }
      rows[[row_i]] <- meta_row(
        fields[[field_i]],
        form,
        field_label = fields[[field_i]],
        branching = branching,
        required = "y"
      )
      row_i <- row_i + 1L
    }
  }

  dplyr::bind_rows(rows)
}

make_benchmark_mapping <- function(forms, events) {
  expand.grid(
    unique_event_name = events,
    form = forms,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(arm_num = 1L, .before = 1L)
}

make_benchmark_events <- function(events) {
  tibble::tibble(
    arm_num = 1L,
    unique_event_name = events,
    event_name = paste("Event", seq_along(events)),
    custom_event_label = ""
  )
}

make_repeat_map <- function(forms, repeat_event) {
  tibble::tibble(
    event_name = repeat_event,
    form_name = forms,
    custom_form_label = ""
  )
}

make_context_rows <- function(n_records, events = NULL, repeat_event = NULL, instances = NULL) {
  ids <- sprintf("record_%05d", seq_len(n_records))
  if (is.null(events)) {
    return(tibble::tibble(record_id = ids))
  }

  repeat_events <- if (is.null(repeat_event)) {
    character()
  } else {
    repeat_event
  }
  regular_events <- setdiff(events, repeat_events)
  regular_rows <- expand.grid(
    record_id = ids,
    redcap_event_name = regular_events,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  if (is.null(repeat_event)) {
    return(tibble::as_tibble(regular_rows))
  }

  repeat_rows <- expand.grid(
    record_id = ids,
    redcap_event_name = repeat_event,
    redcap_repeat_instance = as.character(instances),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  repeat_rows$redcap_repeat_instrument <- ""
  repeat_rows <- repeat_rows[
    c("record_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")
  ]
  repeat_rows$redcap_repeat_instrument <- NA_character_

  regular_rows$redcap_repeat_instrument <- ""
  regular_rows$redcap_repeat_instance <- ""
  dplyr::bind_rows(tibble::as_tibble(regular_rows), tibble::as_tibble(repeat_rows))
}

fill_repeat_instrument <- function(records, forms, repeat_event) {
  if (is.null(repeat_event) || !"redcap_repeat_instrument" %in% names(records)) {
    return(records)
  }
  repeat_rows <- records$redcap_event_name == repeat_event
  records$redcap_repeat_instrument[repeat_rows] <- forms[[1]]
  records
}

make_benchmark_records <- function(
  n_records,
  forms,
  fields_per_form,
  events = NULL,
  repeat_event = NULL,
  instances = NULL
) {
  records <- make_context_rows(
    n_records = n_records,
    events = events,
    repeat_event = repeat_event,
    instances = instances
  )
  records <- fill_repeat_instrument(records, forms, repeat_event)

  n_rows <- nrow(records)
  row_index <- seq_len(n_rows)
  columns <- vector("list", length(forms) * fields_per_form)
  names(columns) <- unlist(lapply(forms, make_field_names, n_fields = fields_per_form))

  for (field_i in seq_along(columns)) {
    blank_every <- 7L + field_i %% 11L
    columns[[field_i]] <- ifelse(
      row_index %% blank_every == 0L,
      "",
      paste0("value_", field_i)
    )
  }

  dplyr::bind_cols(records, tibble::as_tibble(columns))
}

make_tier <- function(tier) {
  switch(
    tier,
    small = {
      forms <- make_form_names(1L)
      list(
        forms = forms,
        records = make_benchmark_records(500L, forms, 25L),
        rcon = fake_rcon(make_benchmark_meta(forms, 25L)),
        instances = NULL
      )
    },
    medium = {
      forms <- make_form_names(3L)
      events <- sprintf("event_%02d", 1:3)
      list(
        forms = forms,
        records = make_benchmark_records(5000L, forms, 100L, events = events),
        rcon = fake_rcon(
          make_benchmark_meta(forms, 100L, longitudinal_branches = TRUE),
          events = make_benchmark_events(events),
          mapping = make_benchmark_mapping(forms, events)
        ),
        instances = NULL
      )
    },
    large = {
      forms <- make_form_names(5L)
      events <- sprintf("event_%02d", 1:3)
      list(
        forms = forms,
        records = make_benchmark_records(
          25000L,
          forms,
          250L,
          events = events,
          repeat_event = "event_03",
          instances = 1:2
        ),
        rcon = fake_rcon(
          make_benchmark_meta(forms, 250L, longitudinal_branches = TRUE),
          events = make_benchmark_events(events),
          mapping = make_benchmark_mapping(forms, events),
          repeat_instrument_event = make_repeat_map(forms, "event_03")
        ),
        instances = 1:2
      )
    },
    stop("Unknown tier: ", tier, call. = FALSE)
  )
}

measure_with_alloc_signal <- function(expr) {
  alloc_path <- tempfile("redcapmissing-profmem-", fileext = ".out")
  alloc_mb <- NA_real_
  use_profmem <- isTRUE(capabilities("profmem"))
  if (use_profmem) {
    utils::Rprofmem(alloc_path)
  }
  value <- force(expr)
  if (use_profmem) {
    utils::Rprofmem(NULL)
    alloc_lines <- readLines(alloc_path, warn = FALSE)
    alloc_bytes <- suppressWarnings(as.numeric(sub(" .*", "", alloc_lines)))
    alloc_mb <- sum(alloc_bytes, na.rm = TRUE) / 1024^2
  }
  unlink(alloc_path)

  list(value = value, allocated_mb = alloc_mb)
}

measure_tier <- function(tier, iteration) {
  input <- make_tier(tier)
  gc(verbose = FALSE)
  measurement <- NULL
  elapsed <- system.time({
    measurement <- measure_with_alloc_signal(
      find_missing(
        data = input$records,
        rcon = input$rcon,
        forms = input$forms,
        instances = input$instances,
        details = FALSE,
        progress = FALSE
      )
    )
    report <- measurement$value
  })[["elapsed"]]
  report <- measurement$value

  tibble::tibble(
    tier = tier,
    iteration = iteration,
    records = nrow(input$records),
    forms = length(input$forms),
    elapsed_seconds = unname(elapsed),
    returned_report_size_mb = as.numeric(utils::object.size(report)) / 1024^2,
    allocated_mb = measurement$allocated_mb,
    summary_rows = nrow(report$summary),
    missing_rows = nrow(report$missing),
    validation_rows = report$diagnostics$validation_rows
  )
}

results <- dplyr::bind_rows(lapply(requested_tiers, function(tier) {
  dplyr::bind_rows(lapply(seq_len(bench_iterations), function(iteration) {
    message("Running ", tier, " iteration ", iteration, " of ", bench_iterations)
    measure_tier(tier, iteration)
  }))
}))

summary <- results |>
  dplyr::group_by(.data$tier) |>
  dplyr::summarise(
    iterations = dplyr::n(),
    records = max(.data$records),
    forms = max(.data$forms),
    median_seconds = stats::median(.data$elapsed_seconds),
    min_seconds = min(.data$elapsed_seconds),
    max_seconds = max(.data$elapsed_seconds),
    median_returned_report_size_mb = stats::median(.data$returned_report_size_mb),
    median_allocated_mb = stats::median(.data$allocated_mb, na.rm = TRUE),
    median_validation_rows = stats::median(.data$validation_rows),
    .groups = "drop"
  )

baseline_path <- Sys.getenv("REDCAPMISSING_BENCH_BASELINE", "")
if (nzchar(baseline_path) && file.exists(baseline_path)) {
  baseline <- utils::read.csv(baseline_path)
  size_col <- if ("median_returned_report_size_mb" %in% names(baseline)) {
    "median_returned_report_size_mb"
  } else {
    "median_report_size_mb"
  }
  baseline <- baseline[
    c("tier", "median_seconds", size_col)
  ]
  names(baseline) <- c(
    "tier",
    "baseline_seconds",
    "baseline_returned_report_size_mb"
  )
  summary <- dplyr::left_join(summary, baseline, by = "tier") |>
    dplyr::mutate(
      time_ratio_vs_baseline = .data$median_seconds / .data$baseline_seconds,
      returned_size_ratio_vs_baseline =
        .data$median_returned_report_size_mb /
          .data$baseline_returned_report_size_mb
    )
}

print(summary, n = Inf)

out_path <- Sys.getenv("REDCAPMISSING_BENCH_OUT", "")
if (nzchar(out_path)) {
  utils::write.csv(summary, out_path, row.names = FALSE)
  message("Wrote benchmark summary: ", out_path)
}
