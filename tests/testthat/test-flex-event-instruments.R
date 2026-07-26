flex_event_instruments_report <- function() {
  target_results <- tibble::tibble(
    record_id = c("r1", "r2", "r1", "r3", "r4"),
    instrument = c("alpha", "alpha", "repeat", "beta", "beta"),
    redcap_event_name = c(
      "baseline_event", "baseline_event", "baseline_event",
      "followup_event", "followup_event"
    ),
    repeat_instrument = c(NA_character_, NA_character_, "repeat", NA_character_, NA_character_),
    repeat_instance = c(NA_integer_, NA_integer_, 2L, NA_integer_, NA_integer_),
    target_source = c("observed", "observed", "extended", "extended", "observed+extended"),
    event_row_started = c("passed", "passed", "passed", "failed", "passed"),
    repeat_instance_row_started = c(
      "not applicable", "not applicable", "failed", "not reached", "not applicable"
    ),
    instrument_started = c("passed", "failed", "not reached", "not reached", "passed"),
    field_complete = c("passed", "not reached", "not reached", "not reached", "failed"),
    fields_assessed = c(2L, 0L, 0L, 0L, 4L),
    fields_failed = c(0L, 0L, 0L, 0L, 2L),
    field_applicability_reason = rep(NA_character_, 5)
  )
  summary <- tibble::tibble(
    redcap_event_name = character(), instrument = character(),
    repeat_instrument = character(), repeat_instance = integer(),
    validation_level = character(), validation_check = character(),
    status = character(), reason = character(), assessed = integer(),
    passed = integer(), failed = integer(), pass_rate = numeric(), fail_rate = numeric()
  )
  plan <- structure(
    list(
      schema_version = 1L,
      construction = "from_data",
      instruments = c("alpha", "repeat", "beta"),
      assessible_targets = target_results[c(
        "record_id", "instrument", "redcap_event_name", "repeat_instrument",
        "repeat_instance", "target_source"
      )],
      project = list(
        project_id = "1", record_id_field = "record_id", longitudinal = TRUE,
        event_labels = c(baseline_event = "Baseline", followup_event = "Follow-up"),
        instrument_labels = c(alpha = "Alpha", beta = "Beta", "repeat" = "Repeat")
      ),
      structure_fingerprint = strrep("0", 64L)
    ),
    class = "redcapmissing_plan"
  )
  complete_report_fixture(
    plan = plan,
    target_results = target_results,
    summary = summary
  )
}

test_that("flex_event_instruments replaces the retired form API", {
  expect_identical(
    as.list(formals(flex_event_instruments)),
    alist(x = , missing_threshold = 0.10, ... = )
  )
  expect_false(exists("flex_event_forms", envir = asNamespace("redcapmissing"), inherits = FALSE))
})

test_that("event-instrument data is computed from frozen target results", {
  parts <- .redcapmissing_flex_event_instruments_build(
    flex_event_instruments_report(),
    missing_threshold = 0.10
  )
  result <- parts$data

  expect_identical(
    parts$display_columns,
    c(
      "Event", "Instrument", "Repeat Instrument", "Repeat Instance", "N",
      "Instrument Incomplete", "Instrument Not Started",
      "Instrument Missing Threshold"
    )
  )
  expect_identical(parts$missing_threshold_heading, "Instrument >10% Missing")
  expect_identical(result$row_type, c("all", "event", "instrument", "instrument", "event", "instrument"))
  expect_identical(result$Event, c("All", "Baseline", "", "", "Follow-up", ""))
  expect_identical(result$Instrument, c("", "", "Alpha", "Repeat", "", "Beta"))
  expect_identical(result$N, c("", "2/2 (100%)", "", "0/1 (0%)", "1/2 (50%)", ""))
  expect_identical(result$`Instrument Incomplete`, c("4/5 (80%)", "", "1/2 (50%)", "1/1 (100%)", "", "2/2 (100%)"))
  expect_identical(result$`Instrument Not Started`, c("3/5 (60%)", "", "1/2 (50%)", "1/1 (100%)", "", "1/2 (50%)"))
  expect_identical(result$`Instrument Missing Threshold`, c("4/5 (80%)", "", "1/2 (50%)", "1/1 (100%)", "", "2/2 (100%)"))
})

test_that("missing-threshold comparison is strict below one", {
  result <- .redcapmissing_flex_event_instruments_build(
    flex_event_instruments_report(),
    missing_threshold = 0.5
  )$data

  expect_identical(
    result$`Instrument Missing Threshold`[[1]],
    "3/5 (60%)"
  )
  expect_identical(
    .redcapmissing_flex_event_instruments_threshold_heading(1),
    "Instrument = 100% Missing"
  )
})

test_that("event-instrument formatter validates its public threshold", {
  for (value in list(NA_real_, Inf, -0.1, 1.1, numeric(), c(0.1, 0.2), "0.1")) {
    expect_error(
      .redcapmissing_flex_event_instruments_build(
        flex_event_instruments_report(),
        missing_threshold = value
      ),
      "missing_threshold"
    )
  }
})

test_that("event-instrument formatter rejects malformed target results", {
  report <- flex_event_instruments_report()
  report$target_results$repeat_instance <- as.character(report$target_results$repeat_instance)
  expect_error(
    .redcapmissing_flex_event_instruments_build(report),
    "storage types"
  )

  report <- flex_event_instruments_report()
  report$target_results$instrument_started[[1]] <- "indeterminate"
  expect_error(
    .redcapmissing_flex_event_instruments_build(report),
    "unsupported check statuses"
  )
})

test_that("flex_event_instruments returns a flextable when dependencies exist", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("glue")

  result <- flex_event_instruments.redcapmissing(flex_event_instruments_report())
  expect_s3_class(result, "flextable")
  expect_true("Instrument" %in% names(result$body$dataset))
  expect_false("Form" %in% names(result$body$dataset))
})