engine_field_checks <- function(report) {
  report$details$checks[["field-complete"]]
}

engine_form_checks <- function(report) {
  report$details$checks[["form-started"]]
}

canonical_legacy_report <- function(report) {
  legacy_diagnostic_names <- c(
    "started_at",
    "finished_at",
    "elapsed_seconds",
    "forms_processed",
    "validation_rows",
    "summary_rows",
    "missing_rows"
  )
  diagnostics <- report$diagnostics[legacy_diagnostic_names]
  diagnostics$started_at <- as.POSIXct(
    "2000-01-01 00:00:00",
    tz = "UTC"
  )
  diagnostics$finished_at <- as.POSIXct(
    "2000-01-01 00:00:00",
    tz = "UTC"
  )
  diagnostics$elapsed_seconds <- 0

  list(
    class = class(report),
    names = names(report),
    summary = report$summary,
    missing = report$missing,
    spec = report$spec,
    diagnostics = diagnostics,
    details = report$details
  )
}

test_that("stage timers accept a deterministic monotonic clock", {
  ticks <- c(10, 10.25, 20, 21.5)
  clock <- function() {
    value <- ticks[[1]]
    ticks <<- ticks[-1]
    value
  }
  timer <- redcapmissing:::.miss_new_timer(clock = clock)

  report_started <- redcapmissing:::.miss_timer_start(timer)
  redcapmissing:::.miss_timer_record(
    timer = timer,
    scope = "report",
    stage = "metadata_api",
    started_at = report_started
  )
  form_started <- redcapmissing:::.miss_timer_start(timer)
  redcapmissing:::.miss_timer_record(
    timer = timer,
    scope = "form",
    form = "baseline_form",
    stage = "blankness",
    started_at = form_started
  )
  rows <- redcapmissing:::.miss_timer_rows(timer)

  expect_identical(
    names(rows),
    c("scope", "form", "stage", "elapsed_seconds")
  )
  expect_equal(rows$scope, c("report", "form"))
  expect_equal(rows$form, c(NA_character_, "baseline_form"))
  expect_equal(rows$stage, c("metadata_api", "blankness"))
  expect_equal(rows$elapsed_seconds, c(0.25, 1.5))
})

test_that("engine diagnostics expose report stages and per-form workloads", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "engine_form", required = "y"),
    meta_row("plain_field", "engine_form", required = "y"),
    meta_row(
      "branched_field",
      "engine_form",
      branching = "[plain_field] = 'go'",
      required = "y"
    ),
    meta_row(
      "plain_checkbox",
      "engine_form",
      field_type = "checkbox",
      choices = "1, First | 2, Second",
      required = "y"
    ),
    meta_row(
      "branched_checkbox",
      "engine_form",
      field_type = "checkbox",
      choices = "1, First | 2, Second",
      branching = "[plain_field] = 'go'",
      required = "y"
    )
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    plain_field = c("go", "go"),
    branched_field = c("entered", ""),
    plain_checkbox___1 = c("1", "0"),
    plain_checkbox___2 = c("0", "1"),
    branched_checkbox___1 = c("1", "0"),
    branched_checkbox___2 = c("0", "1")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "engine_form",
    details = TRUE,
    progress = FALSE
  )
  timings <- report$diagnostics$stage_timings
  workload <- report$diagnostics$form_workload

  expect_s3_class(timings, "tbl_df")
  expect_identical(
    names(timings),
    c("scope", "form", "stage", "elapsed_seconds")
  )
  expect_type(timings$scope, "character")
  expect_type(timings$form, "character")
  expect_type(timings$stage, "character")
  expect_type(timings$elapsed_seconds, "double")
  expect_true(all(is.finite(timings$elapsed_seconds)))
  expect_true(all(timings$elapsed_seconds >= 0))
  expect_setequal(unique(timings$scope), c("report", "form"))
  expect_true(all(c(
    "metadata_api",
    "project_context_api",
    "plan_compilation",
    "url_construction",
    "report_assembly"
  ) %in% timings$stage[timings$scope == "report"]))
  expect_true(all(c(
    "context",
    "eligibility",
    "row_checks",
    "blankness",
    "form_started",
    "ordinary_unbranched",
    "ordinary_branched",
    "checkbox_unbranched",
    "checkbox_branched",
    "form_assembly"
  ) %in% timings$stage[timings$scope == "form"]))
  expect_true(all(is.na(timings$form[timings$scope == "report"])))
  expect_equal(
    unique(timings$form[timings$scope == "form"]),
    "engine_form"
  )

  expect_s3_class(workload, "tbl_df")
  expect_identical(names(workload), c(
    "form",
    "record_rows",
    "expected_contexts",
    "started_rows",
    "total_fields",
    "assessable_fields",
    "ordinary_unbranched_fields",
    "ordinary_branched_fields",
    "checkbox_unbranched_fields",
    "checkbox_branched_fields",
    "validation_rows"
  ))
  expect_equal(nrow(workload), 1L)
  expect_equal(workload$form, "engine_form")
  expect_equal(workload$record_rows, 2L)
  expect_equal(workload$expected_contexts, 2L)
  expect_equal(workload$started_rows, 2L)
  expect_equal(workload$total_fields, 4L)
  expect_equal(workload$assessable_fields, 5L)
  expect_equal(workload$ordinary_unbranched_fields, 2L)
  expect_equal(workload$ordinary_branched_fields, 1L)
  expect_equal(workload$checkbox_unbranched_fields, 1L)
  expect_equal(workload$checkbox_branched_fields, 1L)
  expect_equal(workload$validation_rows, nrow(report$details$validation_rows))
})

test_that("diagnostic stages and workload rows stay form-specific", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "short_form", required = "y"),
    meta_row("short_value", "short_form", required = "y"),
    meta_row("long_value_1", "long_form", required = "y"),
    meta_row("long_value_2", "long_form", required = "y"),
    meta_row("long_value_3", "long_form", required = "y")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    short_value = c("entered", ""),
    long_value_1 = c("entered", "entered"),
    long_value_2 = c("", "entered"),
    long_value_3 = c("entered", "")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = c("short_form", "long_form"),
    details = TRUE,
    progress = FALSE
  )
  timings <- report$diagnostics$stage_timings
  form_timings <- timings[timings$scope == "form", , drop = FALSE]
  workload <- report$diagnostics$form_workload
  required_stages <- c(
    "context",
    "eligibility",
    "row_checks",
    "blankness",
    "form_started",
    "ordinary_unbranched",
    "ordinary_branched",
    "checkbox_unbranched",
    "checkbox_branched",
    "form_assembly"
  )

  expect_setequal(workload$form, c("short_form", "long_form"))
  expect_equal(
    workload$total_fields[match(c("short_form", "long_form"), workload$form)],
    c(1L, 3L)
  )
  expect_equal(
    workload$assessable_fields[
      match(c("short_form", "long_form"), workload$form)
    ],
    c(2L, 3L)
  )
  for (form in c("short_form", "long_form")) {
    form_rows <- form_timings[form_timings$form == form, , drop = FALSE]
    expect_true(all(required_stages %in% form_rows$stage))
    expect_false(anyDuplicated(form_rows$stage) > 0L)
  }
})

test_that("form startedness uses all data fields but never the record ID", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "presence_form", required = "y"),
    meta_row("required_note", "presence_form", required = "y"),
    meta_row("optional_note", "presence_form"),
    meta_row("ignored_note", "presence_form", required = "y"),
    meta_row(
      "excluded_value",
      "presence_form",
      field_type = "calc",
      required = "y"
    )
  )
  records <- tibble::tibble(
    record_id = c("id_only", "optional_only", "ignored_only", "excluded_only"),
    required_note = "",
    optional_note = c("", "entered", "", ""),
    ignored_note = c("", "", "entered", ""),
    excluded_value = c("", "", "", "42")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "presence_form",
    ignore_fields = "ignored_note",
    exclude_types = "calc",
    details = TRUE,
    progress = FALSE
  )
  form_checks <- engine_form_checks(report)
  field_checks <- engine_field_checks(report)

  expected_started <- c(
    id_only = FALSE,
    optional_only = TRUE,
    ignored_only = TRUE,
    excluded_only = TRUE
  )
  expect_equal(
    stats::setNames(
      form_checks$validation_passed,
      form_checks$record_id
    )[names(expected_started)],
    expected_started
  )
  expect_false(any(field_checks$record_id == "id_only"))
  expect_setequal(
    field_checks$record_id[field_checks$field_name == "required_note"],
    c("optional_only", "ignored_only", "excluded_only")
  )
  expect_false(any(field_checks$field_name %in% c(
    "optional_note",
    "ignored_note",
    "excluded_value"
  )))
})

test_that("ordinary field blocks preserve storage-specific blankness and order", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "ordinary_form", required = "y"),
    meta_row("start_marker", "ordinary_form", required = "y"),
    meta_row("character_field", "ordinary_form", required = "y"),
    meta_row("whitespace_field", "ordinary_form", required = "y"),
    meta_row("factor_field", "ordinary_form", required = "y"),
    meta_row("date_field", "ordinary_form", required = "y"),
    meta_row("list_field", "ordinary_form", required = "y"),
    meta_row("absent_field", "ordinary_form", required = "y")
  )
  records <- tibble::tibble(
    record_id = c("blank", "entered"),
    start_marker = c("yes", "yes"),
    character_field = c("", "entered"),
    whitespace_field = c(" \t", "entered"),
    factor_field = factor(
      c("", "entered"),
      levels = c("", "entered")
    ),
    date_field = as.Date(c(NA_character_, "2026-01-15")),
    list_field = list(NA_character_, "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "ordinary_form",
    details = TRUE,
    progress = FALSE
  )
  checks <- engine_field_checks(report)
  data_fields <- c(
    "start_marker",
    "character_field",
    "whitespace_field",
    "factor_field",
    "date_field",
    "list_field",
    "absent_field"
  )
  data_checks <- checks[checks$field_name %in% data_fields, , drop = FALSE]

  expect_equal(
    data_checks$field_name,
    rep(data_fields, each = 2L)
  )
  expect_equal(
    data_checks$record_id,
    rep(c("blank", "entered"), times = length(data_fields))
  )
  expect_equal(
    data_checks$validation_passed,
    c(
      TRUE, TRUE,
      FALSE, TRUE,
      TRUE, TRUE,
      FALSE, TRUE,
      FALSE, TRUE,
      FALSE, TRUE,
      FALSE, FALSE
    )
  )
  expect_equal(
    data_checks$value_summary[data_checks$field_name == "date_field"],
    c(NA_character_, "2026-01-15")
  )
})

test_that("checkbox blocks preserve encodings summaries and branch references", {
  choices <- paste0(seq_len(4), ", Choice ", seq_len(4), collapse = " | ")
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "checkbox_engine", required = "y"),
    meta_row("start_marker", "checkbox_engine", required = "y"),
    meta_row(
      "selection",
      "checkbox_engine",
      field_type = "checkbox",
      choices = choices,
      required = "y"
    ),
    meta_row(
      "conditional_note",
      "checkbox_engine",
      branching = "[selection(2)] = '1'",
      required = "y"
    )
  )
  records <- tibble::tibble(
    record_id = paste0("r", seq_len(6)),
    start_marker = "yes",
    selection___1 = c("1", "checked", "TRUE", "yes", "0", "0"),
    selection___2 = c("0", "0", "0", "0", "unchecked", "1"),
    selection___3 = c("0", "0", "0", "0", "false", "0"),
    selection___4 = c("0", "0", "0", "0", "no", "0"),
    conditional_note = ""
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "checkbox_engine",
    details = TRUE,
    progress = FALSE
  )
  checks <- engine_field_checks(report)
  checkbox_checks <- checks[checks$field_name == "selection", , drop = FALSE]
  conditional_checks <- checks[
    checks$field_name == "conditional_note",
    ,
    drop = FALSE
  ]

  expect_equal(checkbox_checks$record_id, records$record_id)
  expect_equal(
    checkbox_checks$validation_passed,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    checkbox_checks$value_summary,
    c(
      "selection___1",
      "selection___1",
      "selection___1",
      "selection___1",
      "",
      "selection___2"
    )
  )
  expect_equal(conditional_checks$record_id, "r6")
  expect_false(conditional_checks$validation_passed)
  expect_equal(conditional_checks$branch_satisfied, TRUE)
})

test_that("checkbox roots with unequal choice counts remain single checks", {
  checkbox_meta <- function(field, n_choices) {
    meta_row(
      field,
      "checkbox_widths",
      field_type = "checkbox",
      choices = paste0(
        seq_len(n_choices),
        ", Choice ",
        seq_len(n_choices),
        collapse = " | "
      ),
      required = "y"
    )
  }
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "checkbox_widths", required = "y"),
    meta_row("start_marker", "checkbox_widths", required = "y"),
    checkbox_meta("choice_2", 2L),
    checkbox_meta("choice_4", 4L),
    checkbox_meta("choice_8", 8L)
  )
  records <- tibble::tibble(
    record_id = c("selected", "unchecked"),
    start_marker = "yes"
  )
  for (width in c(2L, 4L, 8L)) {
    for (choice in seq_len(width)) {
      records[[paste0("choice_", width, "___", choice)]] <- c(
        if (choice == width) "1" else "0",
        "0"
      )
    }
  }

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "checkbox_widths",
    details = TRUE,
    progress = FALSE
  )
  checks <- engine_field_checks(report)
  checks <- checks[
    checks$field_name %in% c("choice_2", "choice_4", "choice_8"),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(checks), 6L)
  expect_equal(
    checks$field_name,
    rep(c("choice_2", "choice_4", "choice_8"), each = 2L)
  )
  expect_equal(checks$record_id, rep(c("selected", "unchecked"), 3L))
  expect_equal(checks$validation_passed, rep(c(TRUE, FALSE), 3L))
  expect_equal(
    checks$value_summary[checks$record_id == "selected"],
    c("choice_2___2", "choice_4___4", "choice_8___8")
  )
})

test_that("event-qualified branch caches align independently across forms", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "form_a", required = "y"),
    meta_row("screening_gate", "form_a", field_type = "yesno", required = "y"),
    meta_row("a_started", "form_a", required = "y"),
    meta_row(
      "a_conditional",
      "form_a",
      branching = "[screening_event][screening_gate] = '1'",
      required = "y"
    ),
    meta_row("b_started", "form_b", required = "y"),
    meta_row(
      "b_conditional",
      "form_b",
      branching = "[screening_event][screening_gate] = '1'",
      required = "y"
    )
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1, 1),
    unique_event_name = c(
      "screening_event",
      "baseline_event",
      "baseline_event"
    ),
    form = c("form_a", "form_a", "form_b")
  )
  records <- tibble::tibble(
    record_id = rep(c("open", "closed"), each = 2L),
    redcap_event_name = rep(c("screening_event", "baseline_event"), 2L),
    screening_gate = c("1", "", "0", ""),
    a_started = c("", "yes", "", "yes"),
    a_conditional = "",
    b_started = c("", "yes", "", "yes"),
    b_conditional = ""
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata, mapping = mapping),
    forms = c("form_a", "form_b"),
    events = "baseline_event",
    details = TRUE,
    progress = FALSE
  )
  checks <- engine_field_checks(report)
  conditional <- checks[
    checks$field_name %in% c("a_conditional", "b_conditional"),
    ,
    drop = FALSE
  ]

  expect_equal(conditional$form, c("form_a", "form_b"))
  expect_equal(conditional$record_id, c("open", "open"))
  expect_equal(
    conditional$redcap_event_name,
    rep("baseline_event", nrow(conditional))
  )
  expect_false(any(conditional$validation_passed))
  expect_false(any(
    checks$record_id == "closed" &
      checks$field_name %in% c("a_conditional", "b_conditional")
  ))
})

test_that("compact and detailed engines agree for mixed field partitions", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "mixed_engine", required = "y"),
    meta_row("flag", "mixed_engine", field_type = "yesno", required = "y"),
    meta_row("plain", "mixed_engine", required = "y"),
    meta_row(
      "branched",
      "mixed_engine",
      branching = "[flag] = '1'",
      required = "y"
    ),
    meta_row(
      "check_plain",
      "mixed_engine",
      field_type = "checkbox",
      choices = "1, First | 2, Second",
      required = "y"
    ),
    meta_row(
      "check_branched",
      "mixed_engine",
      field_type = "checkbox",
      choices = "1, First | 2, Second",
      branching = "[flag] = '1'",
      required = "y"
    )
  )
  records <- tibble::tibble(
    record_id = c("open", "closed"),
    flag = c("1", "0"),
    plain = c("", "entered"),
    branched = c("", ""),
    check_plain___1 = c("1", "0"),
    check_plain___2 = c("0", "1"),
    check_branched___1 = c("0", "0"),
    check_branched___2 = c("1", "0")
  )
  args <- list(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "mixed_engine",
    progress = FALSE
  )
  compact <- do.call(find_missing, c(args, list(details = FALSE)))
  detailed <- do.call(find_missing, c(args, list(details = TRUE)))

  expect_identical(compact$summary, detailed$summary)
  expect_identical(compact$missing, detailed$missing)
  expect_equal(
    compact$diagnostics$validation_rows,
    nrow(detailed$details$validation_rows)
  )
  expect_identical(
    compact$diagnostics$form_workload,
    detailed$diagnostics$form_workload
  )
  expect_null(compact$details)
})

test_that("optimized engine matches the frozen pre-refactor report contract", {
  metadata <- baseline_form_meta()
  records <- tibble::tibble(
    record_id = c("open", "closed", "unstarted"),
    branch_flag = c("1", "0", ""),
    required_note = c("", "entered", ""),
    optional_note = c("entered", "", ""),
    checkbox_field___1 = c("0", "1", "0"),
    checkbox_field___2 = c("1", "0", "0"),
    checkbox_other = c("", "", ""),
    conditional_note = c("", "", "")
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "baseline_form",
    details = TRUE,
    progress = FALSE
  )

  # Frozen from the pre-engine 3db64f8 implementation. Exact identity covers
  # tibble classes and column types, row ordering, validation row IDs, summary
  # and missing rows, detailed checks/failures, eligibility, and denominators.
  expected <- dget(testthat::test_path(
    "fixtures",
    "find-missing-pre-refactor-canonical.R"
  ))

  expect_identical(canonical_legacy_report(report), expected)
})

test_that("connection accessors stay bounded and backend caches stay warm", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "form_a", required = "y"),
    meta_row("field_a", "form_a", required = "y"),
    meta_row("field_b", "form_b", required = "y")
  )
  accessor_calls <- new.env(parent = emptyenv())
  backend_calls <- new.env(parent = emptyenv())
  warmed <- new.env(parent = emptyenv())
  method_names <- c(
    "metadata",
    "instruments",
    "events",
    "mapping",
    "repeat",
    "project",
    "version"
  )
  for (method in method_names) {
    accessor_calls[[method]] <- 0L
    backend_calls[[method]] <- 0L
    warmed[[method]] <- FALSE
  }
  cached_accessor <- function(method, value) {
    force(method)
    force(value)
    function() {
      accessor_calls[[method]] <- accessor_calls[[method]] + 1L
      if (!isTRUE(warmed[[method]])) {
        backend_calls[[method]] <- backend_calls[[method]] + 1L
        warmed[[method]] <- TRUE
      }
      value
    }
  }
  records_export_calls <- 0L
  rcon <- list(
    url = "https://redcap.example.edu/api/",
    metadata = cached_accessor("metadata", metadata),
    instruments = cached_accessor(
      "instruments",
      tibble::tibble(
        instrument_name = c("form_a", "form_b"),
        instrument_label = c("Form A", "Form B")
      )
    ),
    events = cached_accessor("events", tibble::tibble()),
    mapping = cached_accessor("mapping", tibble::tibble()),
    repeatInstrumentEvent = cached_accessor("repeat", tibble::tibble()),
    projectInformation = cached_accessor(
      "project",
      tibble::tibble(project_id = "1", is_longitudinal = 0L)
    ),
    version = cached_accessor("version", "14.2.0"),
    exportRecordsTyped = function(...) {
      records_export_calls <<- records_export_calls + 1L
      stop("find_missing() must not export records", call. = FALSE)
    }
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    field_a = c("entered", ""),
    field_b = c("", "entered")
  )

  find_missing(
    data = records,
    rcon = rcon,
    forms = c("form_a", "form_b"),
    progress = FALSE
  )
  first_accessor_counts <- vapply(
    method_names,
    function(method) accessor_calls[[method]],
    integer(1)
  )
  first_backend_counts <- vapply(
    method_names,
    function(method) backend_calls[[method]],
    integer(1)
  )

  find_missing(
    data = records,
    rcon = rcon,
    forms = c("form_a", "form_b"),
    progress = FALSE
  )
  second_accessor_counts <- vapply(
    method_names,
    function(method) accessor_calls[[method]],
    integer(1)
  )
  second_backend_counts <- vapply(
    method_names,
    function(method) backend_calls[[method]],
    integer(1)
  )

  expect_true(all(first_accessor_counts <= 1L))
  expect_true(all(second_accessor_counts <= 2L))
  expect_identical(first_backend_counts, second_backend_counts)
  expect_true(all(first_backend_counts <= 1L))
  expect_identical(records_export_calls, 0L)
})
