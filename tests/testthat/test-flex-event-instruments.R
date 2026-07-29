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
        event_labels = c(baseline_event = "Baseline", followup_event = "Follow up"),
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

test_that("event and instrument data is computed from target_results", {
  parts <- .flex_event_instruments_build_table(
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
  expect_identical(result$Event, c("All", "Baseline", "", "", "Follow up", ""))
  expect_identical(result$Instrument, c("", "", "Alpha", "Repeat", "", "Beta"))
  expect_identical(result$N, c("", "2/2 (100%)", "", "0/1 (0%)", "1/2 (50%)", ""))
  expect_identical(result$`Instrument Incomplete`, c("4/5 (80%)", "", "1/2 (50%)", "1/1 (100%)", "", "2/2 (100%)"))
  expect_identical(result$`Instrument Not Started`, c("3/5 (60%)", "", "1/2 (50%)", "1/1 (100%)", "", "1/2 (50%)"))
  expect_identical(result$`Instrument Missing Threshold`, c("4/5 (80%)", "", "1/2 (50%)", "1/1 (100%)", "", "2/2 (100%)"))
})

test_that("missing threshold comparison is strict below one", {
  result <- .flex_event_instruments_build_table(
    flex_event_instruments_report(),
    missing_threshold = 0.5
  )$data

  expect_identical(
    result$`Instrument Missing Threshold`[[1]],
    "3/5 (60%)"
  )
  expect_identical(
    .flex_event_instruments_build_threshold_heading(1),
    "Instrument = 100% Missing"
  )
})

test_that("integer and double one use identical missing threshold semantics", {
  report <- flex_event_instruments_report()
  integer_parts <- .flex_event_instruments_build_table(
    report,
    missing_threshold = 1L
  )
  double_parts <- .flex_event_instruments_build_table(
    report,
    missing_threshold = 1
  )

  expect_identical(integer_parts$data, double_parts$data)
  expect_identical(
    integer_parts$missing_threshold_heading,
    "Instrument = 100% Missing"
  )
  expect_identical(
    integer_parts$data$`Instrument Missing Threshold`[[1L]],
    "3/5 (60%)"
  )
})

test_that("event and instrument formatter validates its public threshold", {
  for (value in list(NA_real_, Inf, -0.1, 1.1, numeric(), c(0.1, 0.2), "0.1")) {
    expect_error(
      .flex_event_instruments_build_table(
        flex_event_instruments_report(),
        missing_threshold = value
      ),
      "missing_threshold"
    )
  }
})

test_that("event and instrument formatter rejects malformed target results", {
  report <- flex_event_instruments_report()
  report$target_results$repeat_instance <- as.character(report$target_results$repeat_instance)
  expect_error(
    .flex_event_instruments_build_table(report),
    "storage types"
  )

  report <- flex_event_instruments_report()
  report$target_results$instrument_started[[1]] <- "indeterminate"
  expect_error(
    .flex_event_instruments_build_table(report),
    "unsupported check statuses"
  )
})

test_that("flex_event_instruments returns a flextable when dependencies exist", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("glue")

  result <- flex_event_instruments(flex_event_instruments_report())
  expect_s3_class(result, "flextable")
  expect_true("Instrument" %in% names(result$body$dataset))
  expect_false("Form" %in% names(result$body$dataset))
  expect_true(all(
    c("Repeat Instrument", "Repeat Instance") %in%
      names(result$body$dataset)
  ))
})

test_that("public dispatch omits repeat columns for classic reports", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("glue")

  rcon <- run_plan_rcon()
  data <- run_plan_data()
  report <- run_plan(
    plan_from_data(data, rcon, "baseline_form"),
    data,
    rcon,
    progress = FALSE
  )
  result <- flex_event_instruments(report)

  expect_s3_class(result, "flextable")
  expect_false(any(
    c("Repeat Instrument", "Repeat Instance") %in%
      names(result$body$dataset)
  ))
  expect_identical(
    names(result$body$dataset),
    c(
      "Event", "Instrument", "N (started/due)",
      "Instrument Incomplete", "Instrument Not Started",
      "Instrument >10% Missing"
    )
  )
})

test_that("non-applicable field checks have zero missingness at threshold zero", {
  report <- flex_event_instruments_report()
  target <- report$target_results[1, , drop = FALSE]
  target$field_complete <- "not applicable"
  target$fields_assessed <- 0L
  target$fields_failed <- 0L
  target$field_applicability_reason <- "no fields remain after field policy"
  report$target_results <- target
  report$plan$assessible_targets <- target[c(
    "record_id", "instrument", "redcap_event_name", "repeat_instrument",
    "repeat_instance", "target_source"
  )]

  result <- .flex_event_instruments_build_table(
    report,
    missing_threshold = 0
  )$data
  instrument_row <- result[result$row_type == "instrument", ]

  expect_identical(result$`Instrument Incomplete`[[1]], "0/1 (0%)")
  expect_identical(result$`Instrument Not Started`[[1]], "0/1 (0%)")
  expect_identical(result$`Instrument Missing Threshold`[[1]], "0/1 (0%)")
  expect_identical(instrument_row$`Instrument Incomplete`, "0/1 (0%)")
  expect_identical(instrument_row$`Instrument Not Started`, "0/1 (0%)")
  expect_identical(
    instrument_row$`Instrument Missing Threshold`,
    "0/1 (0%)"
  )
})
test_that("aggregates many target contexts accurately", {
  record_count <- 200L
  instance_count <- 40L
  grid <- expand.grid(
    record_id = sprintf("r%03d", seq_len(record_count)),
    repeat_instance = seq_len(instance_count),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  failed <- as.integer(sub("r", "", grid$record_id)) %% 10L == 0L
  targets <- tibble::tibble(
    record_id = grid$record_id,
    instrument = rep("repeat", nrow(grid)),
    redcap_event_name = rep("baseline_event", nrow(grid)),
    repeat_instrument = rep("repeat", nrow(grid)),
    repeat_instance = as.integer(grid$repeat_instance),
    target_source = rep("observed", nrow(grid)),
    event_row_started = rep("passed", nrow(grid)),
    repeat_instance_row_started = rep("passed", nrow(grid)),
    instrument_started = rep("passed", nrow(grid)),
    field_complete = ifelse(failed, "failed", "passed"),
    fields_assessed = rep(2L, nrow(grid)),
    fields_failed = as.integer(failed),
    field_applicability_reason = rep(NA_character_, nrow(grid))
  )
  report <- flex_event_instruments_report()
  report$target_results <- targets
  report$plan$instruments <- "repeat"
  report$plan$assessible_targets <- targets[c(
    "record_id", "instrument", "redcap_event_name", "repeat_instrument",
    "repeat_instance", "target_source"
  )]

  result <- .flex_event_instruments_build_table(report)$data

  expect_identical(
    result$row_type,
    c("all", "event", rep("instrument", instance_count))
  )
  expect_identical(
    result$`Repeat Instance`[-c(1L, 2L)],
    as.character(seq_len(instance_count))
  )
  expect_identical(result$N[[2L]], "200/200 (100%)")
  expect_true(all(result$N[-c(1L, 2L)] == "200/200 (100%)"))
  expect_identical(result$`Instrument Incomplete`[[1L]], "800/8000 (10%)")
  expect_true(all(
    result$`Instrument Incomplete`[-c(1L, 2L)] == "20/200 (10%)"
  ))
})

test_that("event aggregation detects conflicting record and event gates", {
  report <- flex_event_instruments_report()
  report$target_results$event_row_started[[3L]] <- "failed"

  expect_error(
    .flex_event_instruments_build_table(report),
    "Conflicting event-row-started"
  )
})

test_that("classic event gates remain fully due when not applicable", {
  report <- flex_event_instruments_report()
  targets <- report$target_results[1:2, , drop = FALSE]
  targets$redcap_event_name <- NA_character_
  targets$event_row_started <- "not applicable"
  targets$repeat_instance_row_started <- "not applicable"
  targets$repeat_instrument <- NA_character_
  targets$repeat_instance <- NA_integer_
  report$target_results <- targets
  report$plan$assessible_targets <- targets[c(
    "record_id", "instrument", "redcap_event_name", "repeat_instrument",
    "repeat_instance", "target_source"
  )]

  result <- .flex_event_instruments_build_table(report)$data

  expect_identical(result$Event, c("All", "Single event", ""))
  expect_identical(result$N, c("", "2/2 (100%)", ""))
})
