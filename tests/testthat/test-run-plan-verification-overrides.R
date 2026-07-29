test_that("exact latest VERIFIED evidence overrides only a failed field check", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()
  result <- run_plan(
    plan, data, rcon, ignore_fields = c("record_id", "branch_flag", "checkbox_field",
      "conditional_note"), verified = issue, verified_user = "alice",
    details = TRUE, progress = FALSE
  )
  field <- result$details[result$details$field_name %in% "required_note", ]
  expect_identical(field$raw_disposition, "failed")
  expect_true(field$verification_applied)
  expect_identical(field$effective_disposition, "passed")
  expect_identical(result$verification$overrides_applied, 1L)
  expect_identical(result$target_results$field_complete, "passed")
  field_summary <- result$summary[
    result$summary$validation_check == "field-complete",
  ]
  expect_identical(
    field_summary[c(
      "status", "reason", "assessed", "passed", "failed",
      "pass_rate", "fail_rate"
    )],
    tibble::tibble(
      status = "assessed",
      reason = NA_character_,
      assessed = 1L,
      passed = 1L,
      failed = 0L,
      pass_rate = 1,
      fail_rate = 0
    )
  )
  expect_false(any(result$missing$field_name == "required_note", na.rm = TRUE))
  expect_false(any(
    get_missing(result)$field_name == "required_note",
    na.rm = TRUE
  ))
  expect_identical(field$reason, NA_character_)
})

test_that("verification preserves a failed repeat instance gate", {
  repeat_table <- tibble::tibble(event_name = NA_character_, form_name = "baseline_form")
  rcon <- run_plan_rcon(repeat_table = repeat_table)
  data <- dplyr::mutate(
    run_plan_data(required_note = ""),
    redcap_repeat_instrument = "baseline_form",
    redcap_repeat_instance = 1L
  )
  schedule <- tibble::tibble(
    record_id = "1", instrument = "baseline_form",
    redcap_event_name = NA_character_, repeat_instance = 2L
  )
  plan <- plan_explicit(data, rcon, "baseline_form", schedule)
  issue <- run_plan_verified_row()
  issue$repeat_instrument <- "baseline_form"
  issue$instance <- 2L
  result <- run_plan(plan, data, rcon, verified = issue,
                     verified_user = "alice", progress = FALSE)
  expect_identical(result$target_results$repeat_instance_row_started, "failed")
  expect_identical(result$target_results$instrument_started, "not reached")
  expect_identical(result$target_results$field_complete, "not reached")
  expect_identical(result$verification$overrides_applied, 0L)
})

test_that("verification cannot bypass a failed instrument gate", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(
    required_note = "",
    start_marker = "",
    branch_flag = "",
    checkbox_1 = "0",
    checkbox_2 = "0"
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- run_plan_verified_row(field_name = "required_note")
  result <- run_plan(
    plan,
    data,
    rcon,
    verified = evidence,
    verified_user = "alice",
    progress = FALSE
  )

  expect_identical(result$target_results$instrument_started, "failed")
  expect_identical(result$target_results$field_complete, "not reached")
  expect_identical(result$verification$overrides_applied, 0L)
})

test_that("verification ignores fields removed before assessment", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(branch_flag = "0")
  plan <- plan_from_data(data, rcon, "baseline_form")
  evidence <- dplyr::bind_rows(
    run_plan_verified_row(field_name = "conditional_note"),
    run_plan_verified_row(field_name = "optional_note")
  )
  result <- run_plan(
    plan,
    data,
    rcon,
    verified = evidence,
    verified_user = "alice",
    details = TRUE,
    progress = FALSE
  )

  expect_identical(result$verification$verified_rows, 2L)
  expect_identical(result$verification$overrides_applied, 0L)
  assessed_fields <- result$details$field_name[
    result$details$validation_check == "field-complete"
  ]
  expect_false(any(c("conditional_note", "optional_note") %in% assessed_fields))
})

test_that("verification ignores fields excluded by field policy", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(branch_flag = "0")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = "required_note",
    verified = run_plan_verified_row(field_name = "required_note"),
    verified_user = "alice",
    progress = FALSE
  )

  expect_identical(result$verification$verified_rows, 1L)
  expect_identical(result$verification$overrides_applied, 0L)
})

test_that("disabled verification records exact zero audit values", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(
    result$verification,
    list(
      enabled = FALSE,
      verified_user = NA_character_,
      input_rows = 0L,
      user_rows = 0L,
      latest_user_rows = 0L,
      verified_rows = 0L,
      overrides_applied = 0L
    )
  )
})

test_that("empty verification input records exact zero audit counts", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  template <- tibble::tibble(
    project_id = logical(),
    record = integer(),
    event_id = as.Date(character()),
    field_name = complex(),
    repeat_instrument = list(),
    instance = raw(),
    ts = as.POSIXct(character(), tz = "UTC"),
    current_query_status = factor(),
    username = double()
  )
  result <- run_plan(
    plan,
    data,
    rcon,
    verified = template,
    verified_user = "alice",
    progress = FALSE
  )

  expect_identical(
    result$verification,
    list(
      enabled = TRUE,
      verified_user = "alice",
      input_rows = 0L,
      user_rows = 0L,
      latest_user_rows = 0L,
      verified_rows = 0L,
      overrides_applied = 0L
    )
  )
})

test_that("verification leaves passing fields unchanged", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "complete")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()
  result <- run_plan(
    plan,
    data,
    rcon,
    verified = issue,
    verified_user = "alice",
    details = TRUE,
    progress = FALSE
  )

  expect_identical(result$verification$overrides_applied, 0L)
  field <- result$details[result$details$field_name %in% "required_note", ]
  expect_false(field$verification_applied)
})

test_that("verification cannot bypass a failed event gate", {
  rcon <- run_plan_repeat_event_rcon()
  data <- run_plan_repeat_event_data()
  schedule <- tibble::tibble(
    record_id = "1",
    instrument = "diary",
    redcap_event_name = "visit_arm_1",
    repeat_instance = 2L
  )
  plan <- plan_explicit(data, rcon, "diary", schedule)
  evidence <- run_plan_verified_row(
    record = "1",
    field_name = "diary_value"
  )
  evidence$event_id <- 102L
  evidence$instance <- 2L
  result <- run_plan(
    plan,
    data,
    rcon,
    verified = evidence,
    verified_user = "alice",
    progress = FALSE
  )

  expect_identical(result$target_results$event_row_started, "failed")
  expect_identical(
    result$target_results$repeat_instance_row_started,
    "not reached"
  )
  expect_identical(result$target_results$instrument_started, "not reached")
  expect_identical(result$target_results$field_complete, "not reached")
  expect_identical(result$verification$overrides_applied, 0L)
})
