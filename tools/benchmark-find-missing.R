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

# Run timing and allocation passes independently. Rprofmem materially changes
# elapsed time, so a profiled call is never used as a timing observation.
parse_bool_env <- function(name, default = FALSE) {
  value <- tolower(trimws(Sys.getenv(name, as.character(default))))
  if (!value %in% c("false", "true", "0", "1", "no", "yes")) {
    stop(
      name,
      " must be true/false, yes/no, or 1/0.",
      call. = FALSE
    )
  }
  value %in% c("true", "1", "yes")
}

parse_positive_integer_env <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
  if (is.na(value) || value < 1L) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  value
}

parse_csv_env <- function(name, default) {
  values <- strsplit(Sys.getenv(name, default), ",", fixed = TRUE)[[1]]
  values <- trimws(values)
  unique(values[nzchar(values)])
}

bench_iterations <- parse_positive_integer_env(
  "REDCAPMISSING_BENCH_ITERATIONS",
  5L
)
bench_seed <- parse_positive_integer_env("REDCAPMISSING_BENCH_SEED", 20260722L)
bench_progress <- parse_bool_env("REDCAPMISSING_BENCH_PROGRESS", FALSE)
bench_warmup <- parse_bool_env("REDCAPMISSING_BENCH_WARMUP", TRUE)
bench_session <- Sys.getenv(
  "REDCAPMISSING_BENCH_SESSION",
  paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "-pid", Sys.getpid())
)

bench_mode <- tolower(trimws(Sys.getenv(
  "REDCAPMISSING_BENCH_MODE",
  "timing"
)))
if (!bench_mode %in% c("timing", "allocation", "both")) {
  stop(
    "REDCAPMISSING_BENCH_MODE must be timing, allocation, or both.",
    call. = FALSE
  )
}
run_timing <- bench_mode %in% c("timing", "both")
run_allocation <- bench_mode %in% c("allocation", "both")

requested_tiers <- parse_csv_env(
  "REDCAPMISSING_BENCH_TIERS",
  "reported_unequal,reported_balanced"
)

make_form_names <- function(n_forms) {
  sprintf("form_%02d", seq_len(n_forms))
}

make_field_names <- function(form, n_fields) {
  sprintf("%s_field_%03d", form, seq_len(n_fields))
}

make_choices <- function(n_choices) {
  paste(
    sprintf("%d, Choice %d", seq_len(n_choices), seq_len(n_choices)),
    collapse = " | "
  )
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

make_repeat_map <- function(form, event) {
  tibble::tibble(
    event_name = event,
    form_name = form,
    custom_form_label = ""
  )
}

make_regular_context_rows <- function(n_records, events = NULL) {
  ids <- sprintf("record_%05d", seq_len(n_records))
  if (is.null(events)) {
    return(tibble::tibble(record_id = ids))
  }

  expand.grid(
    record_id = ids,
    redcap_event_name = events,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble()
}

bind_columns <- function(records, columns) {
  if (length(columns) == 0L) {
    return(records)
  }
  dplyr::bind_cols(records, tibble::as_tibble(columns))
}

add_unrelated_columns <- function(records, n_columns) {
  if (n_columns == 0L) {
    return(records)
  }
  values <- vector("list", n_columns)
  names(values) <- sprintf("unrelated_export_%04d", seq_len(n_columns))
  row_index <- seq_len(nrow(records))
  for (column_i in seq_along(values)) {
    values[[column_i]] <- ifelse(
      (row_index + column_i) %% 17L == 0L,
      "",
      paste0("unrelated_", column_i)
    )
  }
  bind_columns(records, values)
}

make_plain_metadata <- function(forms, field_counts) {
  rows <- list(
    meta_row("record_id", forms[[1]], field_label = "Record ID", required = "y")
  )
  row_i <- 2L
  for (form_i in seq_along(forms)) {
    form <- forms[[form_i]]
    fields <- make_field_names(form, field_counts[[form_i]])
    for (field in fields) {
      rows[[row_i]] <- meta_row(field, form, required = "y")
      row_i <- row_i + 1L
    }
  }
  dplyr::bind_rows(rows)
}

make_plain_records <- function(
  n_records,
  forms,
  field_counts,
  events = NULL,
  n_unrelated = 0L
) {
  records <- make_regular_context_rows(n_records, events)
  row_index <- seq_len(nrow(records))
  columns <- list()
  for (form_i in seq_along(forms)) {
    fields <- make_field_names(forms[[form_i]], field_counts[[form_i]])
    for (field_i in seq_along(fields)) {
      blank_every <- 9L + (field_i + form_i) %% 13L
      columns[[fields[[field_i]]]] <- ifelse(
        row_index %% blank_every == 0L,
        "",
        paste0("value_", form_i, "_", field_i)
      )
    }
  }
  records <- bind_columns(records, columns)
  add_unrelated_columns(records, n_unrelated)
}

make_scenario <- function(
  name,
  records,
  metadata,
  forms,
  events = NULL,
  mapping = NULL,
  repeat_map = NULL,
  instances = NULL,
  details = FALSE,
  expected_validation_rows = NA_integer_
) {
  list(
    name = name,
    records = records,
    metadata = metadata,
    rcon = fake_rcon(
      metadata,
      events = if (is.null(events)) NULL else make_benchmark_events(events),
      mapping = mapping,
      repeat_instrument_event = repeat_map
    ),
    forms = forms,
    events = events,
    instances = instances,
    details = details,
    expected_validation_rows = expected_validation_rows
  )
}

make_reported_scenario <- function(name, field_counts) {
  forms <- make_form_names(length(field_counts))
  # The first form's requested count includes REDCap's record identifier.
  # The other counts are assessable fields. Together they yield 188
  # always-assessed metadata fields, including record_id.
  non_id_counts <- field_counts
  non_id_counts[[1]] <- non_id_counts[[1]] - 1L
  metadata <- make_plain_metadata(forms, non_id_counts)
  gate <- make_field_names(
    forms[[length(forms)]],
    non_id_counts[[length(forms)]]
  )[[1]]
  metadata$field_type[metadata$field_name == gate] <- "yesno"
  conditional <- paste0(forms[[length(forms)]], "_conditional")
  metadata <- dplyr::bind_rows(
    metadata,
    meta_row(
      conditional,
      forms[[length(forms)]],
      branching = sprintf("[%s] = '1'", gate),
      required = "y"
    )
  )

  records <- make_plain_records(150L, forms, non_id_counts)
  records[[gate]] <- c(rep("1", 58L), rep("0", 92L))
  records[[conditional]] <- ""

  make_scenario(
    name = name,
    records = records,
    metadata = metadata,
    forms = forms,
    expected_validation_rows = 29758L
  )
}

make_small_scenario <- function() {
  forms <- make_form_names(1L)
  counts <- 25L
  make_scenario(
    name = "small",
    records = make_plain_records(500L, forms, counts),
    metadata = make_plain_metadata(forms, counts),
    forms = forms
  )
}

make_ordinary_wide_scenario <- function() {
  forms <- make_form_names(4L)
  counts <- rep(40L, length(forms))
  make_scenario(
    name = "ordinary_wide",
    records = make_plain_records(
      1000L,
      forms,
      counts,
      n_unrelated = 400L
    ),
    metadata = make_plain_metadata(forms, counts),
    forms = forms
  )
}

make_branch_heavy_scenario <- function() {
  forms <- make_form_names(3L)
  events <- c("event_01", "event_02")
  base_count <- 16L
  branch_count <- 16L
  metadata <- make_plain_metadata(forms, rep(base_count, length(forms)))
  branch_rows <- list()
  branch_i <- 1L

  for (form in forms) {
    gate <- make_field_names(form, base_count)[[1]]
    metadata$field_type[metadata$field_name == gate] <- "yesno"
    for (field_i in seq_len(branch_count)) {
      branching <- switch(
        as.character((field_i - 1L) %% 3L),
        `0` = sprintf("[%s] = '1'", gate),
        `1` = sprintf("[%s] = '1' and [%s] <> ''", gate, gate),
        `2` = sprintf("[event_01][%s] = '1'", gate)
      )
      branch_rows[[branch_i]] <- meta_row(
        sprintf("%s_branch_%03d", form, field_i),
        form,
        branching = branching,
        required = "y"
      )
      branch_i <- branch_i + 1L
    }
  }
  metadata <- dplyr::bind_rows(metadata, dplyr::bind_rows(branch_rows))

  records <- make_plain_records(300L, forms, rep(base_count, 3L), events)
  open_ids <- sprintf("record_%05d", seq_len(150L))
  columns <- list()
  for (form in forms) {
    gate <- make_field_names(form, base_count)[[1]]
    records[[gate]] <- ifelse(records$record_id %in% open_ids, "1", "0")
    for (field_i in seq_len(branch_count)) {
      field <- sprintf("%s_branch_%03d", form, field_i)
      columns[[field]] <- ifelse(
        seq_len(nrow(records)) %% (7L + field_i %% 5L) == 0L,
        "",
        paste0("branch_value_", field_i)
      )
    }
  }
  records <- bind_columns(records, columns)

  make_scenario(
    name = "branch_heavy",
    records = records,
    metadata = metadata,
    forms = forms,
    events = events,
    mapping = make_benchmark_mapping(forms, events)
  )
}

make_checkbox_heavy_scenario <- function() {
  forms <- make_form_names(2L)
  ordinary_count <- 8L
  choice_counts <- c(2L, 4L, 8L, 2L, 4L, 8L)
  metadata <- make_plain_metadata(forms, rep(ordinary_count, length(forms)))
  checkbox_rows <- list()
  checkbox_i <- 1L
  child_columns <- list()
  n_records <- 750L
  row_index <- seq_len(n_records)

  records <- make_plain_records(
    n_records,
    forms,
    rep(ordinary_count, length(forms))
  )
  for (form_i in seq_along(forms)) {
    form <- forms[[form_i]]
    first_root <- NULL
    for (root_i in seq_along(choice_counts)) {
      root <- sprintf("%s_checkbox_%02d", form, root_i)
      if (is.null(first_root)) {
        first_root <- root
      }
      n_choices <- choice_counts[[root_i]]
      checkbox_rows[[checkbox_i]] <- meta_row(
        root,
        form,
        field_type = "checkbox",
        choices = make_choices(n_choices),
        required = "y"
      )
      checkbox_i <- checkbox_i + 1L
      selected_choice <- (row_index + root_i + form_i) %% n_choices + 1L
      for (choice in seq_len(n_choices)) {
        child <- sprintf("%s___%d", root, choice)
        unchecked <- switch(
          as.character((choice - 1L) %% 5L),
          `0` = "0",
          `1` = "unchecked",
          `2` = "false",
          `3` = "no",
          `4` = ""
        )
        child_columns[[child]] <- ifelse(
          selected_choice == choice,
          "1",
          unchecked
        )
      }
    }
    checkbox_rows[[checkbox_i]] <- meta_row(
      sprintf("%s_checkbox_branch", form),
      form,
      branching = sprintf("[%s(1)] = '1'", first_root),
      required = "y"
    )
    checkbox_i <- checkbox_i + 1L
  }
  metadata <- dplyr::bind_rows(metadata, dplyr::bind_rows(checkbox_rows))
  records <- bind_columns(records, child_columns)
  for (form in forms) {
    records[[sprintf("%s_checkbox_branch", form)]] <- ""
  }

  make_scenario(
    name = "checkbox_heavy",
    records = records,
    metadata = metadata,
    forms = forms,
    details = TRUE
  )
}

make_longitudinal_omission_scenario <- function() {
  forms <- make_form_names(2L)
  events <- sprintf("event_%02d", 1:3)
  counts <- c(20L, 30L)
  records <- make_plain_records(300L, forms, counts, events)
  id_number <- as.integer(sub("record_", "", records$record_id, fixed = TRUE))
  records <- records[
    records$redcap_event_name != "event_03" | id_number %% 3L != 0L,
    ,
    drop = FALSE
  ]

  make_scenario(
    name = "longitudinal_omission",
    records = records,
    metadata = make_plain_metadata(forms, counts),
    forms = forms,
    events = events,
    mapping = make_benchmark_mapping(forms, events)
  )
}

make_mixed_repeat_scenario <- function() {
  form <- "repeat_form"
  event <- "baseline_event"
  ids <- sprintf("record_%05d", seq_len(300L))
  contexts <- expand.grid(
    record_id = ids,
    redcap_repeat_instance = as.character(1:3),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      .id_number = as.integer(sub("record_", "", .data$record_id, fixed = TRUE)),
      .instance_number = as.integer(.data$redcap_repeat_instance)
    ) |>
    dplyr::filter(
      .data$.instance_number == 1L |
        (.data$.instance_number == 2L & .data$.id_number %% 4L != 0L) |
        (.data$.instance_number == 3L & .data$.id_number %% 3L == 0L)
    ) |>
    dplyr::transmute(
      record_id = .data$record_id,
      redcap_event_name = event,
      redcap_repeat_instrument = form,
      redcap_repeat_instance = .data$redcap_repeat_instance
    )
  fields <- sprintf("repeat_field_%03d", seq_len(30L))
  columns <- lapply(seq_along(fields), function(field_i) {
    ifelse(
      seq_len(nrow(contexts)) %% (8L + field_i %% 9L) == 0L,
      "",
      paste0("repeat_value_", field_i)
    )
  })
  names(columns) <- fields
  records <- bind_columns(contexts, columns)
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "identifier_form", required = "y"),
    dplyr::bind_rows(lapply(fields, meta_row, form_name = form, required = "y"))
  )
  mapping <- tibble::tibble(
    arm_num = 1L,
    unique_event_name = event,
    form = form
  )

  make_scenario(
    name = "mixed_repeat",
    records = records,
    metadata = metadata,
    forms = form,
    events = event,
    mapping = mapping,
    repeat_map = make_repeat_map(form, event),
    instances = 1:3
  )
}

make_realistic_mixed_wide_scenario <- function() {
  forms <- make_form_names(4L)
  events <- c("event_01", "event_02")
  counts <- c(12L, 24L, 36L, 48L)
  metadata <- make_plain_metadata(forms, counts)
  records <- make_plain_records(
    400L,
    forms,
    counts,
    events = events,
    n_unrelated = 300L
  )
  extra_meta <- list()
  extra_columns <- list()
  row_index <- seq_len(nrow(records))
  meta_i <- 1L

  for (form_i in seq_along(forms)) {
    form <- forms[[form_i]]
    gate <- make_field_names(form, counts[[form_i]])[[1]]
    metadata$field_type[metadata$field_name == gate] <- "yesno"
    records[[gate]] <- ifelse(row_index %% (form_i + 2L) != 0L, "1", "0")

    for (branch_i in seq_len(4L)) {
      field <- sprintf("%s_mixed_branch_%02d", form, branch_i)
      extra_meta[[meta_i]] <- meta_row(
        field,
        form,
        branching = if (branch_i %% 2L == 0L) {
          sprintf("[event_01][%s] = '1'", gate)
        } else {
          sprintf("[%s] = '1'", gate)
        },
        required = "y"
      )
      extra_columns[[field]] <- ifelse(row_index %% 11L == 0L, "", "entered")
      meta_i <- meta_i + 1L
    }

    root <- sprintf("%s_mixed_checkbox", form)
    extra_meta[[meta_i]] <- meta_row(
      root,
      form,
      field_type = "checkbox",
      choices = make_choices(4L),
      required = "y"
    )
    for (choice in seq_len(4L)) {
      extra_columns[[sprintf("%s___%d", root, choice)]] <- ifelse(
        (row_index + form_i) %% 4L + 1L == choice,
        "1",
        "0"
      )
    }
    meta_i <- meta_i + 1L
  }
  metadata <- dplyr::bind_rows(metadata, dplyr::bind_rows(extra_meta))
  records <- bind_columns(records, extra_columns)
  id_number <- as.integer(sub("record_", "", records$record_id, fixed = TRUE))
  records <- records[
    records$redcap_event_name != "event_02" | id_number %% 5L != 0L,
    ,
    drop = FALSE
  ]

  make_scenario(
    name = "realistic_mixed_wide",
    records = records,
    metadata = metadata,
    forms = forms,
    events = events,
    mapping = make_benchmark_mapping(forms, events)
  )
}

tier_builders <- list(
  reported_unequal = function() make_reported_scenario(
    "reported_unequal",
    c(4L, 8L, 11L, 14L, 17L, 19L, 21L, 25L, 30L, 39L)
  ),
  reported_balanced = function() make_reported_scenario(
    "reported_balanced",
    c(rep(19L, 8L), 18L, 18L)
  ),
  ordinary_wide = make_ordinary_wide_scenario,
  branch_heavy = make_branch_heavy_scenario,
  checkbox_heavy = make_checkbox_heavy_scenario,
  longitudinal_omission = make_longitudinal_omission_scenario,
  mixed_repeat = make_mixed_repeat_scenario,
  realistic_mixed_wide = make_realistic_mixed_wide_scenario,
  small = make_small_scenario,
  reported = function() make_reported_scenario(
    "reported",
    c(4L, 8L, 11L, 14L, 17L, 19L, 21L, 25L, 30L, 39L)
  ),
  medium = make_branch_heavy_scenario,
  large = make_realistic_mixed_wide_scenario
)

unknown_tiers <- setdiff(requested_tiers, names(tier_builders))
if (length(unknown_tiers) > 0L) {
  stop(
    "Unknown benchmark tier(s): ",
    paste(unknown_tiers, collapse = ", "),
    ". Available tiers: ",
    paste(names(tier_builders), collapse = ", "),
    ".",
    call. = FALSE
  )
}

build_tier <- function(tier) {
  input <- tier_builders[[tier]]()
  input$name <- tier
  input
}

run_find_missing <- function(input) {
  find_missing(
    data = input$records,
    rcon = input$rcon,
    forms = input$forms,
    events = input$events,
    instances = input$instances,
    details = input$details,
    progress = bench_progress
  )
}

check_report <- function(input, report) {
  expected <- input$expected_validation_rows
  if (!is.na(expected) && report$diagnostics$validation_rows != expected) {
    stop(
      "Tier `",
      input$name,
      "` returned ",
      report$diagnostics$validation_rows,
      " validation rows; expected ",
      expected,
      ".",
      call. = FALSE
    )
  }
  invisible(report)
}

report_metrics <- function(input, report) {
  tibble::tibble(
    records = nrow(input$records),
    unique_records = dplyr::n_distinct(input$records$record_id),
    forms = length(input$forms),
    metadata_fields = nrow(input$metadata) - 1L,
    export_columns = ncol(input$records),
    details = input$details,
    returned_report_size_mb =
      as.numeric(utils::object.size(report)) / 1024^2,
    summary_rows = nrow(report$summary),
    missing_rows = nrow(report$missing),
    validation_rows = report$diagnostics$validation_rows
  )
}

extract_stage_timings <- function(input, report, iteration) {
  timings <- report$diagnostics$stage_timings
  if (!is.data.frame(timings) || nrow(timings) == 0L) {
    return(tibble::tibble())
  }
  dplyr::mutate(
    tibble::as_tibble(timings),
    session = bench_session,
    tier = input$name,
    iteration = iteration,
    progress = bench_progress,
    .before = 1L
  )
}

extract_form_workload <- function(input, report, iteration) {
  workload <- report$diagnostics$form_workload
  if (!is.data.frame(workload) || nrow(workload) == 0L) {
    return(tibble::tibble())
  }
  dplyr::mutate(
    tibble::as_tibble(workload),
    session = bench_session,
    tier = input$name,
    iteration = iteration,
    progress = bench_progress,
    .before = 1L
  )
}

measure_timing <- function(input, iteration) {
  gc(verbose = FALSE)
  report <- NULL
  elapsed <- system.time({
    report <- run_find_missing(input)
  })[["elapsed"]]
  check_report(input, report)
  list(
    result = dplyr::mutate(
      report_metrics(input, report),
      session = bench_session,
      tier = input$name,
      iteration = iteration,
      progress = bench_progress,
      elapsed_seconds = unname(elapsed),
      .before = 1L
    ),
    stage_timings = extract_stage_timings(input, report, iteration),
    form_workload = extract_form_workload(input, report, iteration)
  )
}

measure_allocation <- function(input, iteration) {
  alloc_path <- tempfile("redcapmissing-profmem-", fileext = ".out")
  on.exit(unlink(alloc_path), add = TRUE)
  use_profmem <- isTRUE(capabilities("profmem"))
  if (!use_profmem) {
    warning(
      "Rprofmem is unavailable; allocation results will be NA.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  gc(verbose = FALSE)
  report <- NULL
  profiling <- FALSE
  if (use_profmem) {
    utils::Rprofmem(alloc_path)
    profiling <- TRUE
    on.exit({
      if (profiling) {
        utils::Rprofmem(NULL)
      }
    }, add = TRUE)
  }
  report <- run_find_missing(input)
  if (profiling) {
    utils::Rprofmem(NULL)
    profiling <- FALSE
  }
  check_report(input, report)

  allocated_mb <- NA_real_
  if (use_profmem) {
    alloc_lines <- readLines(alloc_path, warn = FALSE)
    alloc_bytes <- suppressWarnings(as.numeric(sub(" .*", "", alloc_lines)))
    allocated_mb <- sum(alloc_bytes, na.rm = TRUE) / 1024^2
  }
  dplyr::mutate(
    report_metrics(input, report),
    session = bench_session,
    tier = input$name,
    iteration = iteration,
    progress = bench_progress,
    allocated_mb = allocated_mb,
    .before = 1L
  )
}

if (bench_warmup) {
  message("Warming ", length(requested_tiers), " requested tier(s)")
  for (tier in requested_tiers) {
    input <- build_tier(tier)
    check_report(input, run_find_missing(input))
  }
}

timing_results <- list()
allocation_results <- list()
stage_results <- list()
workload_results <- list()
timing_i <- allocation_i <- stage_i <- workload_i <- 1L

set.seed(bench_seed)
if (run_timing) {
  for (iteration in seq_len(bench_iterations)) {
    tier_order <- sample(requested_tiers, length(requested_tiers))
    for (tier in tier_order) {
      message(
        "Timing ",
        tier,
        " iteration ",
        iteration,
        " of ",
        bench_iterations
      )
      measurement <- measure_timing(build_tier(tier), iteration)
      timing_results[[timing_i]] <- measurement$result
      timing_i <- timing_i + 1L
      if (nrow(measurement$stage_timings) > 0L) {
        stage_results[[stage_i]] <- measurement$stage_timings
        stage_i <- stage_i + 1L
      }
      if (nrow(measurement$form_workload) > 0L) {
        workload_results[[workload_i]] <- measurement$form_workload
        workload_i <- workload_i + 1L
      }
    }
  }
}

set.seed(bench_seed + 1L)
if (run_allocation) {
  for (iteration in seq_len(bench_iterations)) {
    tier_order <- sample(requested_tiers, length(requested_tiers))
    for (tier in tier_order) {
      message(
        "Allocating ",
        tier,
        " iteration ",
        iteration,
        " of ",
        bench_iterations
      )
      allocation_results[[allocation_i]] <- measure_allocation(
        build_tier(tier),
        iteration
      )
      allocation_i <- allocation_i + 1L
    }
  }
}

timing_results <- dplyr::bind_rows(timing_results)
allocation_results <- dplyr::bind_rows(allocation_results)
stage_results <- dplyr::bind_rows(stage_results)
workload_results <- dplyr::bind_rows(workload_results)

result_keys <- c("session", "tier", "iteration", "progress")
if (nrow(timing_results) > 0L && nrow(allocation_results) > 0L) {
  allocation_metrics <- dplyr::select(
    allocation_results,
    dplyr::all_of(c(result_keys, "allocated_mb"))
  )
  results <- dplyr::left_join(
    timing_results,
    allocation_metrics,
    by = result_keys
  )
} else if (nrow(timing_results) > 0L) {
  results <- dplyr::mutate(timing_results, allocated_mb = NA_real_)
} else {
  results <- dplyr::mutate(allocation_results, elapsed_seconds = NA_real_)
}

median_or_na <- function(value) {
  if (all(is.na(value))) {
    return(NA_real_)
  }
  stats::median(value, na.rm = TRUE)
}

range_or_na <- function(value, fn) {
  if (all(is.na(value))) {
    return(NA_real_)
  }
  fn(value, na.rm = TRUE)
}

summary <- results |>
  dplyr::group_by(.data$tier, .data$progress) |>
  dplyr::summarise(
    timing_iterations = sum(!is.na(.data$elapsed_seconds)),
    allocation_iterations = sum(!is.na(.data$allocated_mb)),
    records = max(.data$records),
    unique_records = max(.data$unique_records),
    forms = max(.data$forms),
    metadata_fields = max(.data$metadata_fields),
    export_columns = max(.data$export_columns),
    median_seconds = median_or_na(.data$elapsed_seconds),
    min_seconds = range_or_na(.data$elapsed_seconds, min),
    max_seconds = range_or_na(.data$elapsed_seconds, max),
    median_returned_report_size_mb =
      median_or_na(.data$returned_report_size_mb),
    median_allocated_mb = median_or_na(.data$allocated_mb),
    median_validation_rows = median_or_na(.data$validation_rows),
    .groups = "drop"
  )

baseline_path <- Sys.getenv("REDCAPMISSING_BENCH_BASELINE", "")
if (nzchar(baseline_path) && file.exists(baseline_path)) {
  baseline <- tibble::as_tibble(utils::read.csv(baseline_path))
  size_col <- if ("median_returned_report_size_mb" %in% names(baseline)) {
    "median_returned_report_size_mb"
  } else {
    "median_report_size_mb"
  }
  baseline_columns <- c("tier", "median_seconds", size_col)
  if ("median_allocated_mb" %in% names(baseline)) {
    baseline_columns <- c(baseline_columns, "median_allocated_mb")
  }
  baseline <- baseline[baseline_columns]
  names(baseline)[names(baseline) == "median_seconds"] <- "baseline_seconds"
  names(baseline)[names(baseline) == size_col] <-
    "baseline_returned_report_size_mb"
  names(baseline)[names(baseline) == "median_allocated_mb"] <-
    "baseline_allocated_mb"

  summary <- dplyr::left_join(summary, baseline, by = "tier") |>
    dplyr::mutate(
      time_ratio_vs_baseline =
        .data$median_seconds / .data$baseline_seconds,
      returned_size_ratio_vs_baseline =
        .data$median_returned_report_size_mb /
          .data$baseline_returned_report_size_mb
    )
  if ("baseline_allocated_mb" %in% names(summary)) {
    summary <- dplyr::mutate(
      summary,
      allocation_ratio_vs_baseline =
        .data$median_allocated_mb / .data$baseline_allocated_mb
    )
  }
}

stage_summary <- tibble::tibble()
if (nrow(stage_results) > 0L && "elapsed_seconds" %in% names(stage_results)) {
  stage_group_columns <- intersect(
    c("tier", "progress", "scope", "form", "stage"),
    names(stage_results)
  )
  stage_summary <- stage_results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(stage_group_columns))) |>
    dplyr::summarise(
      iterations = dplyr::n(),
      median_seconds = stats::median(.data$elapsed_seconds),
      min_seconds = min(.data$elapsed_seconds),
      max_seconds = max(.data$elapsed_seconds),
      .groups = "drop"
    )
}

print(summary, n = Inf, width = Inf)
if (nrow(stage_summary) > 0L) {
  message("Per-stage timing summary")
  print(stage_summary, n = Inf, width = Inf)
}

derived_output_path <- function(path, suffix) {
  extension <- tools::file_ext(path)
  stem <- tools::file_path_sans_ext(path)
  if (nzchar(extension)) {
    paste0(stem, suffix, ".", extension)
  } else {
    paste0(path, suffix)
  }
}

out_path <- Sys.getenv("REDCAPMISSING_BENCH_OUT", "")
raw_out_path <- Sys.getenv("REDCAPMISSING_BENCH_RAW_OUT", "")
stage_out_path <- Sys.getenv("REDCAPMISSING_BENCH_STAGE_OUT", "")
workload_out_path <- Sys.getenv("REDCAPMISSING_BENCH_WORKLOAD_OUT", "")
if (nzchar(out_path)) {
  utils::write.csv(summary, out_path, row.names = FALSE)
  message("Wrote benchmark summary: ", out_path)
  if (!nzchar(raw_out_path)) {
    raw_out_path <- derived_output_path(out_path, "-raw")
  }
  if (!nzchar(stage_out_path)) {
    stage_out_path <- derived_output_path(out_path, "-stages")
  }
  if (!nzchar(workload_out_path)) {
    workload_out_path <- derived_output_path(out_path, "-workload")
  }
}
if (nzchar(raw_out_path)) {
  utils::write.csv(results, raw_out_path, row.names = FALSE)
  message("Wrote raw benchmark runs: ", raw_out_path)
}
if (nzchar(stage_out_path) && nrow(stage_results) > 0L) {
  utils::write.csv(stage_results, stage_out_path, row.names = FALSE)
  message("Wrote raw stage timings: ", stage_out_path)
}
if (nzchar(workload_out_path) && nrow(workload_results) > 0L) {
  utils::write.csv(workload_results, workload_out_path, row.names = FALSE)
  message("Wrote per-form workload: ", workload_out_path)
}
