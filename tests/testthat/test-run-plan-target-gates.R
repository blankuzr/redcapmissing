test_that("run_plan freezes targets and gates absent physical rows", {
  rcon <- run_plan_rcon()
  data <- run_plan_data("1")
  plan <- plan_explicit(data, rcon, "baseline_form", run_plan_explicit_schedule("2"))
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(result$target_results$record_id, "2")
  expect_identical(result$target_results$event_row_started, "not applicable")
  expect_identical(result$target_results$repeat_instance_row_started, "not applicable")
  expect_identical(result$target_results$instrument_started, "failed")
  expect_identical(result$target_results$field_complete, "not reached")
  expect_true("instrument-started" %in% result$missing$validation_check)
})

test_that("classic repeating targets bypass the event gate and retain the instance gate", {
  repeat_table <- tibble::tibble(
    event_name = NA_character_,
    form_name = "baseline_form"
  )
  rcon <- run_plan_rcon(repeat_table = repeat_table)
  data <- run_plan_data()
  data$redcap_repeat_instrument <- "baseline_form"
  data$redcap_repeat_instance <- 1L
  schedule <- tibble::tibble(
    record_id = c("1", "1"),
    instrument = c("baseline_form", "baseline_form"),
    redcap_event_name = c(NA_character_, NA_character_),
    repeat_instance = c(1L, 2L)
  )
  plan <- plan_explicit(data, rcon, "baseline_form", schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(
    result$target_results$event_row_started,
    c("not applicable", "not applicable")
  )
  expect_identical(
    result$target_results$repeat_instance_row_started,
    c("passed", "failed")
  )
  expect_identical(
    result$target_results$instrument_started,
    c("passed", "not reached")
  )
})

test_that("longitudinal repeating-instrument targets distinguish event and instance absence", {
  repeat_table <- tibble::tibble(
    event_name = "baseline_arm_1",
    form_name = "baseline_form"
  )
  rcon <- run_plan_rcon(
    longitudinal = TRUE,
    repeat_table = repeat_table
  )
  data <- run_plan_data(record_id = "2")
  data$redcap_event_name <- "baseline_arm_1"
  data$redcap_repeat_instrument <- "baseline_form"
  data$redcap_repeat_instance <- 1L
  schedule <- tibble::tibble(
    record_id = c("1", "2"),
    instrument = c("baseline_form", "baseline_form"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
    repeat_instance = c(2L, 2L)
  )
  plan <- plan_explicit(data, rcon, "baseline_form", schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(
    result$target_results$repeat_instrument,
    c("baseline_form", "baseline_form")
  )
  expect_identical(
    result$target_results$event_row_started,
    c("failed", "passed")
  )
  expect_identical(
    result$target_results$repeat_instance_row_started,
    c("not reached", "failed")
  )
  expect_identical(
    result$target_results$instrument_started,
    c("not reached", "not reached")
  )
  expect_identical(
    result$target_results$field_complete,
    c("not reached", "not reached")
  )
})

test_that("longitudinal event gates use any physical row in the record and event", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha", required = "y"),
    meta_row("alpha_value", "alpha"),
    meta_row("beta_value", "beta", required = "y")
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = c("alpha", "beta"),
      instrument_label = c("Alpha", "Beta")
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77", is_longitudinal = 1L
    ),
    arms = function() tibble::tibble(arm_num = 1L, name = "Arm 1"),
    events = function() tibble::tibble(
      event_id = 101L,
      unique_event_name = "baseline_arm_1",
      event_name = "Baseline",
      arm_num = 1L
    ),
    mapping = function() tibble::tibble(
      arm_num = c(1L, 1L),
      unique_event_name = c("baseline_arm_1", "baseline_arm_1"),
      form = c("alpha", "beta")
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(), form_name = character()
    )
  )
  rcon <- redcap_api_connection_fixture(rcon)
  data <- tibble::tibble(
    record_id = "1",
    redcap_event_name = "baseline_arm_1",
    alpha_value = "entered",
    beta_value = ""
  )
  schedule <- tibble::tibble(
    record_id = "1",
    instrument = "beta",
    redcap_event_name = "baseline_arm_1",
    repeat_instance = NA_integer_
  )
  plan <- plan_explicit(data, rcon, "beta", schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(result$target_results$event_row_started, "passed")
  expect_identical(
    result$target_results$repeat_instance_row_started,
    "not applicable"
  )
  expect_identical(result$target_results$instrument_started, "failed")
  expect_identical(result$target_results$field_complete, "not reached")
})

test_that("repeating event gates distinguish absent events from absent instances", {
  rcon <- run_plan_repeat_event_rcon()
  data <- run_plan_repeat_event_data()
  schedule <- tibble::tibble(
    record_id = c("1", "2"),
    instrument = c("diary", "diary"),
    redcap_event_name = c("visit_arm_1", "visit_arm_1"),
    repeat_instance = c(2L, 2L)
  )
  plan <- plan_explicit(data, rcon, "diary", schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(result$target_results$record_id, c("1", "2"))
  expect_identical(
    result$target_results$event_row_started,
    c("failed", "passed")
  )
  expect_identical(
    result$target_results$repeat_instance_row_started,
    c("not reached", "failed")
  )
  expect_identical(
    result$target_results$instrument_started,
    c("not reached", "not reached")
  )
  expect_identical(
    result$target_results$field_complete,
    c("not reached", "not reached")
  )
})

test_that("classic targets mark the event-row gate not applicable", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE)
  event_summary <- result$summary[
    result$summary$validation_check == "event-row-started",
  ]

  expect_identical(event_summary$status, "not applicable")
  expect_identical(event_summary$reason, "not applicable for classic project")
})

test_that("absent frozen rows fail the instrument gate", {
  rcon <- run_plan_rcon()
  planned_data <- run_plan_data(record_id = "1")
  plan <- plan_from_data(planned_data, rcon, "baseline_form")

  absent <- run_plan(
    plan,
    run_plan_data(record_id = "2"),
    rcon,
    progress = FALSE
  )
  expect_identical(absent$target_results$record_id, "1")
  expect_identical(absent$target_results$instrument_started, "failed")
  expect_identical(absent$target_results$field_complete, "not reached")
})

test_that("zero-row runtime data retains explicit targets and gate states", {
  rcon <- run_plan_rcon()
  planner_data <- run_plan_data()
  plan <- plan_explicit(
    planner_data,
    rcon,
    "baseline_form",
    run_plan_explicit_schedule("absent")
  )
  runtime_data <- planner_data[0, , drop = FALSE]
  result <- run_plan(
    plan,
    runtime_data,
    rcon,
    details = TRUE,
    progress = FALSE
  )

  expect_identical(result$target_results$record_id, "absent")
  expect_identical(result$target_results$target_source, "explicit")
  expect_identical(
    result$target_results[c(
      "event_row_started", "repeat_instance_row_started",
      "instrument_started", "field_complete"
    )],
    tibble::tibble(
      event_row_started = "not applicable",
      repeat_instance_row_started = "not applicable",
      instrument_started = "failed",
      field_complete = "not reached"
    )
  )
})
