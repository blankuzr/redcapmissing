flexify_summary_fixture <- function() {
  out <- tibble::tibble(
    redcap_event_name = "baseline_event",
    instrument = "baseline",
    repeat_instrument = NA_character_,
    repeat_instance = NA_integer_,
    validation_level = "event:instrument",
    validation_check = "field-complete",
    status = "not applicable",
    reason = "no assessible fields after field policy",
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
  expect_silent(.redcapmissing_check_flexify_input(flexify_summary_fixture()))
  expect_silent(.redcapmissing_check_flexify_input(flexify_missing_fixture()))

  expect_identical(
    .redcapmissing_flexify_column_types()[c("instrument", "repeat_instance", "status", "reason")],
    c(instrument = "character", repeat_instance = "integer", status = "character", reason = "character")
  )
})

test_that("flexify uses instrument labels and current validation labels", {
  input <- flexify_summary_fixture()
  labels <- attr(input, "redcapmissing_labels")
  transformed <- .redcapmissing_flexify_label_values(input, labels)

  expect_identical(transformed$redcap_event_name, "Baseline")
  expect_identical(transformed$instrument, "Baseline instrument")
  expect_identical(transformed$validation_check, "Field complete")
  expect_identical(.redcapmissing_flexify_header_labels()[["instrument"]], "Instrument")
  expect_identical(.redcapmissing_flexify_header_labels()[["status"]], "Status")
  expect_identical(.redcapmissing_flexify_header_labels()[["reason"]], "Reason")
  expect_false("form" %in% names(.redcapmissing_flexify_header_labels()))
})

test_that("flexify drops jointly absent repeat columns without mutating input", {
  input <- flexify_summary_fixture()
  original <- input
  result <- .redcapmissing_flexify_drop_blank_repeat_columns(input)

  expect_false("repeat_instrument" %in% names(result))
  expect_false("repeat_instance" %in% names(result))
  expect_identical(input, original)
})

test_that("flexify rejects retired columns and columns from different schemas", {
  input <- flexify_summary_fixture()
  input$form <- input$instrument
  expect_error(.redcapmissing_check_flexify_input(input), "unsupported column")

  input <- flexify_summary_fixture()
  input$field_name <- NA_character_
  expect_error(.redcapmissing_check_flexify_input(input), "use columns from one accessor schema")

  input <- flexify_summary_fixture()
  input$repeat_instance <- as.character(input$repeat_instance)
  expect_error(.redcapmissing_check_flexify_input(input), "storage types")
})

test_that("flexify returns a presentation table with N/A rates blank", {
  skip_if_not_installed("flextable")

  result <- flexify(flexify_summary_fixture())
  expect_s3_class(result, "flextable")
  expect_true("instrument" %in% names(result$body$dataset))
  expect_false("form" %in% names(result$body$dataset))
})