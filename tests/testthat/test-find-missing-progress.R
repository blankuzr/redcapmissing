progress_test_fixture <- function() {
  metadata <- dplyr::bind_rows(
    meta_row(
      "record_id",
      "intake_form",
      field_label = "Record ID",
      required = "y"
    ),
    meta_row(
      "intake_value",
      "intake_form",
      field_label = "Intake value",
      required = "y"
    ),
    meta_row(
      "followup_value",
      "followup_form",
      field_label = "Follow-up value",
      required = "y"
    )
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    intake_value = c("entered", ""),
    followup_value = c("", "entered")
  )

  list(
    metadata = metadata,
    args = list(
      data = records,
      rcon = fake_rcon(metadata),
      forms = c("intake_form", "followup_form")
    )
  )
}

progress_without_timing <- function(report) {
  report$diagnostics$started_at <- NULL
  report$diagnostics$finished_at <- NULL
  report$diagnostics$elapsed_seconds <- NULL
  report
}

test_that("progress formatter uses the Native Cool constellation", {
  old_options <- options(
    cli.num_colors = 16777216,
    cli.unicode = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  progress_line <- getFromNamespace(
    ".miss_cli_progress_line",
    "redcapmissing"
  )
  progress_palette <- getFromNamespace(
    ".miss_cli_progress_palette",
    "redcapmissing"
  )
  progress_symbols <- getFromNamespace(
    ".miss_cli_progress_symbols",
    "redcapmissing"
  )

  expect_identical(
    progress_palette(),
    c(
      completed = "#22C55E",
      active = "#22D3EE",
      overall = "#3B82F6",
      pending = "#64748B",
      failed = "#EF4444"
    )
  )
  expect_identical(
    progress_symbols(unicode = TRUE),
    list(
      tick = "\u2713",
      active = "\u25c9",
      pending = "\u25cb",
      failed = "\u2717",
      separator = "\u2502",
      multiply = "\u00d7",
      dot = "\u00b7"
    )
  )
  expect_identical(
    progress_symbols(unicode = FALSE),
    list(
      tick = "v",
      active = "*",
      pending = ".",
      failed = "x",
      separator = "|",
      multiply = "x",
      dot = "-"
    )
  )

  line <- progress_line(
    forms = c("form_1", "form_2", "baseline_form", "form_4", "form_5"),
    form_index = 3,
    form_fraction = 0.60,
    overall_fraction = 0.33,
    phase = "form",
    width = 120
  )
  plain_line <- cli::ansi_strip(line)

  expect_true(cli::ansi_has_any(line))
  expect_identical(
    plain_line,
    "find_missing  \u2713 \u2713 \u25c9 \u25cb \u25cb  baseline_form 60%  \u2502  OVERALL 33%"
  )
  expect_false(grepl("\u2661|\u2665|\u2764", plain_line))
  expect_true(grepl("\033[38;2;34;197;94m", line, fixed = TRUE))
  expect_true(grepl("\033[38;2;34;211;238m", line, fixed = TRUE))
  expect_true(grepl("\033[38;2;59;130;246m", line, fixed = TRUE))
  expect_true(grepl("\033[38;2;100;116;139m", line, fixed = TRUE))

  failed_line <- progress_line(
    forms = c("form_1", "baseline_form", "form_3"),
    form_index = 2,
    form_fraction = 0.40,
    overall_fraction = 0.30,
    phase = "failed",
    width = 120
  )
  expect_identical(
    cli::ansi_strip(failed_line),
    "find_missing  \u2713 \u2717 \u25cb  baseline_form failed at 40%  \u2502  OVERALL 30%"
  )
  expect_true(grepl("\033[38;2;239;68;68m", failed_line, fixed = TRUE))

  compact_line <- progress_line(
    forms = paste0("form_", seq_len(50)),
    form_index = 18,
    form_fraction = 0.60,
    overall_fraction = 0.33,
    phase = "form",
    width = 60
  )
  expect_true(cli::ansi_nchar(compact_line, type = "width") <= 60)
  expect_match(cli::ansi_strip(compact_line), "\u2713\u00d717 \u25c9 \u25cb\u00d732", fixed = TRUE)
  expect_match(cli::ansi_strip(compact_line), "form_18 60%", fixed = TRUE)
  expect_match(cli::ansi_strip(compact_line), "OVERALL 33%", fixed = TRUE)

  narrow_lines <- lapply(
    c("form", "finalizing", "done", "failed", "finalizing_failed"),
    function(phase) {
      progress_line(
        forms = paste0("form_", seq_len(50)),
        form_index = 18,
        form_fraction = 0.60,
        overall_fraction = 0.33,
        phase = phase,
        width = 20
      )
    }
  )
  expect_true(all(vapply(
    narrow_lines,
    function(line) cli::ansi_nchar(line, type = "width") <= 20,
    logical(1)
  )))
  expect_match(cli::ansi_strip(narrow_lines[[1]]), "F60%", fixed = TRUE)
  expect_match(cli::ansi_strip(narrow_lines[[1]]), "O33%", fixed = TRUE)
})

test_that("hybrid form and overall progress math is monotonic", {
  form_fraction <- getFromNamespace(
    ".miss_cli_form_fraction",
    "redcapmissing"
  )
  overall_fraction <- getFromNamespace(
    ".miss_cli_overall_fraction",
    "redcapmissing"
  )
  clamp_fraction <- getFromNamespace(
    ".miss_cli_clamp_fraction",
    "redcapmissing"
  )
  percent <- getFromNamespace(".miss_percent", "redcapmissing")

  stage_names <- c(
    "start",
    "context",
    "metadata",
    "eligibility",
    "row_checks",
    "form_checks",
    "field_complete",
    "complete"
  )
  expect_equal(
    unname(vapply(stage_names, form_fraction, numeric(1))),
    c(0, 0.10, 0.20, 0.35, 0.45, 0.50, 0.95, 1)
  )

  field_fractions <- vapply(
    c(0, 0.25, 0.50, 0.75, 1),
    function(value) {
      form_fraction("field_checks", field_fraction = value)
    },
    numeric(1)
  )
  expect_equal(
    field_fractions,
    c(0.50, 0.6125, 0.7250, 0.8375, 0.95)
  )
  expect_true(all(diff(field_fractions) >= 0))

  form_indexes <- rep(seq_len(4), each = 3)
  within_form <- rep(c(0, 0.50, 1), times = 4)
  overall_values <- mapply(
    overall_fraction,
    form_index = form_indexes,
    form_fraction = within_form,
    MoreArgs = list(form_count = 4)
  )
  expect_equal(
    unname(overall_values),
    c(
      0, 0.11875, 0.23750,
      0.23750, 0.35625, 0.47500,
      0.47500, 0.59375, 0.71250,
      0.71250, 0.83125, 0.95000
    )
  )
  expect_true(all(diff(overall_values) >= 0))
  expect_lt(max(overall_values), 1)
  expect_identical(percent(max(overall_values)), 95L)
  expect_equal(overall_fraction(3, 0.60, 8), 0.30875)
  expect_identical(percent(overall_fraction(3, 0.60, 8)), 31L)

  expect_equal(
    vapply(
      list(-1, 0, 0.50, 2, NA_character_, numeric()),
      clamp_fraction,
      numeric(1)
    ),
    c(0, 0, 0.50, 1, 0, 0)
  )
  expect_error(form_fraction("unknown"), "Unknown progress stage")
})

test_that("form report progress visits every field without changing checks", {
  metadata <- baseline_form_meta()
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "entered",
    optional_note = "",
    checkbox_field___1 = "1",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )
  rcon <- fake_rcon(metadata)

  get_project_cache <- getFromNamespace(
    ".miss_get_project_cache",
    "redcapmissing"
  )
  get_project_events <- getFromNamespace(
    ".miss_get_project_event_names",
    "redcapmissing"
  )
  resolve_records <- getFromNamespace(
    ".miss_resolve_records_arg",
    "redcapmissing"
  )
  build_form_report <- getFromNamespace(
    ".miss_build_form_report",
    "redcapmissing"
  )

  project_cache <- get_project_cache(rcon)
  record_specs <- resolve_records(
    records = NULL,
    valid_events = get_project_events(project_cache = project_cache),
    forms = "baseline_form",
    ignore_ids = character()
  )
  form_args <- list(
    records = records,
    all_records = records,
    meta = metadata,
    rcon = rcon,
    project_cache = project_cache,
    form = "baseline_form",
    events = NULL,
    record_specs = record_specs,
    required_fields = TRUE,
    ignore_fields = NULL,
    exclude_types = "descriptive",
    instances = NULL,
    instances_explicit = FALSE,
    details = TRUE
  )

  quiet_report <- do.call(
    build_form_report,
    c(form_args, list(progress_callback = NULL))
  )
  callback_events <- list()
  progress_callback <- function(form_fraction, force = FALSE) {
    callback_events[[length(callback_events) + 1L]] <<- list(
      form_fraction = form_fraction,
      force = force
    )
  }
  progress_report <- do.call(
    build_form_report,
    c(form_args, list(progress_callback = progress_callback))
  )

  callback_forced <- vapply(
    callback_events,
    `[[`,
    logical(1),
    "force"
  )
  callback_fractions <- vapply(
    callback_events,
    `[[`,
    numeric(1),
    "form_fraction"
  )
  field_fractions <- callback_fractions[!callback_forced]
  field_count <- nrow(progress_report$field_plan)

  expect_equal(progress_report, quiet_report)
  expect_length(field_fractions, field_count)
  expect_equal(
    field_fractions,
    0.50 + 0.45 * seq_len(field_count) / field_count
  )
  expect_setequal(
    setdiff(
      progress_report$field_plan$field_name,
      progress_report$field_complete_checks$field_name
    ),
    c("checkbox_other", "conditional_note")
  )
})

test_that("dynamic progress replaces one line and cleans up on success", {
  fixture <- progress_test_fixture()
  old_options <- options(
    cli.dynamic = TRUE,
    cli.progress_handlers_only = "cli",
    cli.progress_show_after = 0,
    cli.num_colors = 1,
    cli.unicode = FALSE,
    width = 120
  )
  on.exit(options(old_options), add = TRUE)

  active_before <- cli::cli_progress_num()
  on.exit({
    if (cli::cli_progress_num() > active_before) {
      cli::cli_progress_cleanup()
    }
  }, add = TRUE)

  progress_messages <- testthat::capture_messages(
    report <- do.call(
      find_missing,
      c(fixture$args, list(progress = TRUE))
    )
  )
  update_messages <- head(progress_messages, -1L)
  output_stream <- paste0(progress_messages, collapse = "")
  visible_states <- trimws(
    gsub(
      "\r",
      "",
      cli::ansi_strip(update_messages),
      fixed = TRUE
    ),
    which = "right"
  )

  expect_s3_class(report, "redcapmissing")
  expect_identical(active_before, 0L)
  expect_identical(cli::cli_progress_num(), 0L)
  expect_gt(length(update_messages), 1L)
  expect_identical(tail(progress_messages, 1L), "\n")
  expect_true(all(startsWith(update_messages, "\r")))
  expect_true(all(endsWith(update_messages, "\r")))
  expect_true(all(vapply(
    update_messages,
    function(message) {
      sum(charToRaw(message) == as.raw(13)) == 2L
    },
    logical(1)
  )))
  expect_equal(sum(charToRaw(output_stream) == as.raw(10)), 1)
  expect_true(any(grepl(
    "intake_form [0-9]+%",
    visible_states
  )))
  expect_true(any(grepl(
    "followup_form [0-9]+%",
    visible_states
  )))
  expect_true(all(grepl(
    "OVERALL [0-9]+%",
    visible_states
  )))
  expect_false(any(
    visible_states[-1L] == visible_states[-length(visible_states)]
  ))
})

test_that("non-dynamic progress emits only the final completion line", {
  fixture <- progress_test_fixture()
  old_options <- options(
    cli.dynamic = FALSE,
    cli.num_colors = 1,
    cli.unicode = TRUE,
    width = 120
  )
  on.exit(options(old_options), add = TRUE)

  progress_output <- utils::capture.output(
    report <- do.call(
      find_missing,
      c(fixture$args, list(progress = TRUE))
    )
  )

  expect_s3_class(report, "redcapmissing")
  expect_identical(
    progress_output,
    "\u2713 find_missing complete \u00b7 2/2 forms \u00b7 OVERALL 100%"
  )
})

test_that("progress FALSE is silent on output and message streams", {
  fixture <- progress_test_fixture()
  old_options <- options(
    cli.dynamic = TRUE,
    cli.progress_handlers_only = "cli",
    cli.progress_show_after = 0,
    cli.num_colors = 16777216,
    cli.unicode = TRUE
  )
  on.exit(options(old_options), add = TRUE)

  progress_messages <- testthat::capture_messages(
    progress_output <- utils::capture.output(
      report <- do.call(
        find_missing,
        c(fixture$args, list(progress = FALSE))
      )
    )
  )

  expect_s3_class(report, "redcapmissing")
  expect_identical(progress_output, character())
  expect_identical(progress_messages, character())
})

test_that("progress bars are cleaned up when find_missing errors", {
  fixture <- progress_test_fixture()
  invalid_metadata <- fixture$metadata
  invalid_metadata$required_field <- NULL
  invalid_args <- fixture$args
  invalid_args$rcon <- fake_rcon(invalid_metadata)

  old_options <- options(
    cli.dynamic = TRUE,
    cli.progress_handlers_only = "cli",
    cli.progress_show_after = 0,
    cli.num_colors = 1,
    cli.unicode = FALSE
  )
  on.exit(options(old_options), add = TRUE)

  active_before <- cli::cli_progress_num()
  on.exit({
    if (cli::cli_progress_num() > active_before) {
      cli::cli_progress_cleanup()
    }
  }, add = TRUE)

  progress_messages <- testthat::capture_messages(
    failure <- tryCatch(
      do.call(
        find_missing,
        c(invalid_args, list(progress = TRUE))
      ),
      error = identity
    )
  )

  expect_s3_class(failure, "error")
  expect_match(
    conditionMessage(failure),
    "must include `required_field`",
    fixed = TRUE
  )
  expect_identical(cli::cli_progress_num(), active_before)
  expect_match(
    cli::ansi_strip(paste0(progress_messages, collapse = "")),
    "x .  intake_form failed at 10%",
    fixed = TRUE
  )
})

test_that("late failures preserve completed forms and identify report assembly", {
  fixture <- progress_test_fixture()
  old_options <- options(
    cli.dynamic = TRUE,
    cli.progress_handlers_only = "cli",
    cli.progress_show_after = 0,
    cli.num_colors = 1,
    cli.unicode = FALSE,
    width = 120
  )
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    .miss_build_validation_summary = function(...) {
      stop("late assembly failure", call. = FALSE)
    },
    .package = "redcapmissing"
  )

  active_before <- cli::cli_progress_num()
  on.exit({
    if (cli::cli_progress_num() > active_before) {
      cli::cli_progress_cleanup()
    }
  }, add = TRUE)

  progress_messages <- testthat::capture_messages(
    failure <- tryCatch(
      do.call(
        find_missing,
        c(fixture$args, list(progress = TRUE))
      ),
      error = identity
    )
  )
  progress_stream <- cli::ansi_strip(paste0(
    progress_messages,
    collapse = ""
  ))

  expect_s3_class(failure, "error")
  expect_match(conditionMessage(failure), "late assembly failure", fixed = TRUE)
  expect_identical(cli::cli_progress_num(), active_before)
  expect_match(
    progress_stream,
    "v v x  report assembly failed",
    fixed = TRUE
  )
  expect_match(progress_stream, "OVERALL 96%", fixed = TRUE)
  expect_false(grepl("followup_form failed", progress_stream, fixed = TRUE))
})

test_that("progress reporting does not change the returned report", {
  fixture <- progress_test_fixture()
  old_options <- options(
    cli.dynamic = FALSE,
    cli.num_colors = 1,
    cli.unicode = TRUE,
    width = 120
  )
  on.exit(options(old_options), add = TRUE)

  quiet_report <- do.call(
    find_missing,
    c(fixture$args, list(details = TRUE, progress = FALSE))
  )
  progress_output <- utils::capture.output(
    progress_report <- do.call(
      find_missing,
      c(fixture$args, list(details = TRUE, progress = TRUE))
    )
  )

  expect_identical(class(progress_report), class(quiet_report))
  expect_equal(
    progress_without_timing(progress_report),
    progress_without_timing(quiet_report)
  )
  expect_identical(
    progress_output,
    "\u2713 find_missing complete \u00b7 2/2 forms \u00b7 OVERALL 100%"
  )
})
