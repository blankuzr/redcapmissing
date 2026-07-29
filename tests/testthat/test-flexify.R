flexify_public_report <- function() {
  rcon <- run_plan_rcon()
  data <- run_plan_data(
    record_id = c("r1", "r2"),
    required_note = c("", "entered")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  run_plan(plan, data, rcon, progress = FALSE)
}

flexify_summary_fixture <- function() {
  out <- tibble::tibble(
    redcap_event_name = "baseline_event",
    instrument = "baseline",
    repeat_instrument = NA_character_,
    repeat_instance = NA_integer_,
    validation_level = "event:instrument",
    validation_check = "field-complete",
    status = "not applicable",
    reason = "no fields remain after field policy",
    assessed = 0L,
    passed = 0L,
    failed = 0L,
    pass_rate = NA_real_,
    fail_rate = NA_real_
  )
  attr(out, "redcapmissing_labels") <- list(
    events = c(baseline_event = "Baseline"),
    instruments = c(baseline = "Baseline instrument")
  )
  out
}

flexify_missing_fixture <- function() {
  tibble::tibble(
    record_id = "r1",
    redcap_event_name = NA_character_,
    repeat_instrument = NA_character_,
    repeat_instance = NA_integer_,
    validation_context = "classic",
    instrument = "baseline",
    validation_check = "instrument-started",
    field_name = NA_character_,
    field_label = NA_character_,
    field_type = NA_character_,
    branching_logic = NA_character_,
    url = NA_character_
  )
}

test_that("flexify accepts exact summary and missing accessor schemas", {
  expect_silent(.flexify_validate_input(flexify_summary_fixture()))
  expect_silent(.flexify_validate_input(flexify_missing_fixture()))

  expect_identical(
    .flexify_list_column_types()[c("instrument", "repeat_instance", "status", "reason")],
    c(instrument = "character", repeat_instance = "integer", status = "character", reason = "character")
  )
})

test_that("flexify uses instrument labels and current validation labels", {
  input <- flexify_summary_fixture()
  labels <- attr(input, "redcapmissing_labels")
  transformed <- .flexify_apply_labels(input, labels)

  expect_identical(transformed$redcap_event_name, "Baseline")
  expect_identical(transformed$instrument, "Baseline instrument")
  expect_identical(transformed$validation_check, "Field complete")
  expect_identical(.flexify_build_header_labels()[["instrument"]], "Instrument")
  expect_identical(.flexify_build_header_labels()[["status"]], "Status")
  expect_identical(.flexify_build_header_labels()[["reason"]], "Reason")
  expect_false("form" %in% names(.flexify_build_header_labels()))
})

test_that("flexify drops jointly absent repeat columns without mutating input", {
  input <- flexify_summary_fixture()
  original <- input
  result <- .flexify_drop_blank_repeat_columns(input)

  expect_false("repeat_instrument" %in% names(result))
  expect_false("repeat_instance" %in% names(result))
  expect_identical(input, original)
})

test_that("flexify rejects retired columns and columns from different schemas", {
  input <- flexify_summary_fixture()
  input$form <- input$instrument
  expect_error(.flexify_validate_input(input), "unsupported column")

  input <- flexify_summary_fixture()
  input$field_name <- NA_character_
  expect_error(.flexify_validate_input(input), "use columns from one accessor schema")

  input <- flexify_summary_fixture()
  input$repeat_instance <- as.character(input$repeat_instance)
  expect_error(.flexify_validate_input(input), "storage types")
})

test_that("flexify returns a presentation table with N/A rates blank", {
  skip_if_not_installed("flextable")

  result <- flexify(flexify_summary_fixture())
  expect_s3_class(result, "flextable")
  expect_true("instrument" %in% names(result$body$dataset))
  expect_false("form" %in% names(result$body$dataset))
})

test_that("flexify preserves complete public accessor outputs", {
  skip_if_not_installed("flextable")

  report <- flexify_public_report()
  summary_rows <- get_summary(report)
  missing_rows <- get_missing(report)
  original_summary <- summary_rows
  original_missing <- missing_rows

  summary_flex <- flexify(summary_rows)
  missing_flex <- flexify(missing_rows)

  expect_s3_class(summary_flex, "flextable")
  expect_s3_class(missing_flex, "flextable")
  expect_identical(
    names(summary_flex$body$dataset),
    setdiff(
      names(summary_rows),
      c("repeat_instrument", "repeat_instance")
    )
  )
  expect_identical(
    names(missing_flex$body$dataset),
    setdiff(
      names(missing_rows),
      c("repeat_instrument", "repeat_instance")
    )
  )
  expect_identical(summary_rows, original_summary)
  expect_identical(missing_rows, original_missing)
})

test_that("flexify preserves reordered grouped subset and zero-row inputs", {
  skip_if_not_installed("flextable")

  summary_rows <- get_summary(flexify_public_report())
  selected <- summary_rows[
    rev(seq_len(nrow(summary_rows))),
    c("passed", "instrument", "validation_check", "pass_rate"),
    drop = FALSE
  ]
  grouped <- dplyr::group_by(selected, instrument)
  original_grouped <- grouped

  selected_flex <- flexify(grouped)
  one_column_flex <- flexify(
    summary_rows[, "passed", drop = FALSE]
  )
  single_repeat_flex <- flexify(
    summary_rows[, "repeat_instrument", drop = FALSE]
  )
  repeat_only_flex <- flexify(summary_rows[, c(
    "repeat_instance", "repeat_instrument"
  ), drop = FALSE])
  zero_row_flex <- flexify(summary_rows[0, c(
    "instrument", "repeat_instrument", "repeat_instance"
  ), drop = FALSE])

  expect_identical(
    names(selected_flex$body$dataset),
    c("passed", "instrument", "validation_check", "pass_rate")
  )
  expect_identical(selected_flex$body$dataset$passed, selected$passed)
  expect_identical(names(one_column_flex$body$dataset), "passed")
  expect_identical(
    names(single_repeat_flex$body$dataset),
    "repeat_instrument"
  )
  expect_identical(
    names(repeat_only_flex$body$dataset),
    c("repeat_instance", "repeat_instrument")
  )
  expect_identical(names(zero_row_flex$body$dataset), "instrument")
  expect_identical(nrow(zero_row_flex$body$dataset), 0L)
  expect_identical(grouped, original_grouped)
})

test_that("flexify retains repeat context and applies raw label fallback", {
  skip_if_not_installed("flextable")

  input <- tibble::tibble(
    redcap_event_name = c("baseline_event", "unlabeled_event"),
    instrument = c("baseline", "unlabeled_instrument"),
    repeat_instrument = c(NA_character_, "unlabeled_instrument"),
    repeat_instance = c(NA_integer_, 2L)
  )
  attr(input, "redcapmissing_labels") <- list(
    events = c(baseline_event = "Baseline"),
    instruments = c(baseline = "Baseline instrument")
  )
  original <- input
  output <- flexify(input)

  expect_identical(input, original)
  expect_identical(
    names(output$body$dataset),
    names(input)
  )
  expect_identical(
    output$body$dataset$redcap_event_name,
    c("Baseline", "unlabeled_event")
  )
  expect_identical(
    output$body$dataset$instrument,
    c("Baseline instrument", "unlabeled_instrument")
  )
  expect_identical(
    output$body$dataset$repeat_instrument,
    c("", "unlabeled_instrument")
  )
  expect_identical(output$body$dataset$repeat_instance, c(NA_integer_, 2L))

  nonrepeat <- input[1, , drop = FALSE]
  expect_false(any(
    c("repeat_instrument", "repeat_instance") %in%
      names(flexify(nonrepeat)$body$dataset)
  ))
})

test_that("flexify renders labels percentages blanks and URL hyperlinks", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("htmltools")

  summary_input <- tibble::tibble(
    pass_rate = c(0.625, NA_real_),
    validation_check = c("field-complete", "instrument-started"),
    instrument = c("baseline", NA_character_),
    assessed = c(1L, NA_integer_)
  )
  summary_output <- flexify(summary_input)
  summary_html <- flex_html(summary_output)

  expect_identical(
    summary_output$body$dataset$validation_check,
    c("Field complete", "Instrument started")
  )
  expect_match(summary_html, "Pass Rate", fixed = TRUE)
  expect_match(summary_html, "Validation Check", fixed = TRUE)
  expect_match(summary_html, "Instrument", fixed = TRUE)
  expect_match(summary_html, "62.5%", fixed = TRUE)
  expect_false(grepl(">NA<", summary_html, fixed = TRUE))

  missing_input <- tibble::tibble(
    record_id = c("r1", "r2"),
    url = c("https://example.test/records/1", NA_character_),
    field_label = c("Required value", NA_character_)
  )
  missing_output <- flexify(missing_input)
  missing_html <- flex_html(missing_output)

  expect_identical(missing_output$body$dataset$url, missing_input$url)
  expect_match(
    missing_html,
    'href="https://example.test/records/1"',
    fixed = TRUE
  )
  expect_false(grepl(">NA<", missing_html, fixed = TRUE))
})

test_that("formatter dependency diagnostics name every missing package", {
  expect_error(
    .flex_require_packages(
      c(
        "redcapmissing_optional_dependency_a",
        "redcapmissing_optional_dependency_b"
      ),
      "presentation contract"
    ),
    "redcapmissing_optional_dependency_a, redcapmissing_optional_dependency_b"
  )
  expect_error(
    .flex_require_packages(
      "redcapmissing_optional_dependency_a",
      "presentation contract"
    ),
    "presentation contract"
  )
})
