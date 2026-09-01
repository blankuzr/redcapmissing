test_that("longitudinal unresolved rows receive REDCap data entry URLs", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(run_plan_data(required_note = ""),
                        redcap_event_name = "baseline_arm_1")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon,
    ignore_fields = c("record_id", "branch_flag", "checkbox_field", "conditional_note"),
    progress = FALSE)
  field <- result$missing[result$missing$field_name %in% "required_note", ]
  expect_match(field$url, "pid=77", fixed = TRUE)
  expect_match(field$url, "event_id=101", fixed = TRUE)
})

test_that("run_plan result components preserve every documented storage type", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)

  expect_identical(vapply(result$target_results, typeof, character(1)), c(
    record_id = "character", instrument = "character",
    redcap_event_name = "character", repeat_instrument = "character",
    repeat_instance = "integer", target_source = "character",
    event_row_started = "character",
    repeat_instance_row_started = "character",
    instrument_started = "character", field_complete = "character",
    fields_assessed = "integer", fields_failed = "integer",
    field_applicability_reason = "character"
  ))
  expect_identical(vapply(result$summary, typeof, character(1)), c(
    redcap_event_name = "character", instrument = "character",
    repeat_instrument = "character", repeat_instance = "integer",
    validation_level = "character", validation_check = "character",
    status = "character", reason = "character", assessed = "integer",
    passed = "integer", failed = "integer", pass_rate = "double",
    fail_rate = "double"
  ))
  expect_identical(vapply(result$missing, typeof, character(1)), c(
    record_id = "character", redcap_event_name = "character",
    repeat_instrument = "character", repeat_instance = "integer",
    validation_context = "character", instrument = "character",
    validation_check = "character", field_name = "character",
    field_label = "character", field_type = "character",
    branching_logic = "character", url = "character"
  ))
  expect_identical(vapply(result$diagnostics, typeof, character(1)), c(
    stage = "integer", operation = "character", completed = "logical",
    elapsed_seconds = "double"
  ))
  expect_identical(names(result$verification), c(
    "enabled", "verified_user", "input_rows", "user_rows",
    "latest_user_rows", "verified_rows", "overrides_applied"
  ))
  expect_identical(vapply(result$verification, typeof, character(1)), c(
    enabled = "logical", verified_user = "character", input_rows = "integer",
    user_rows = "integer", latest_user_rows = "integer",
    verified_rows = "integer", overrides_applied = "integer"
  ))
})

test_that("zero targets produce empty report components", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(run_plan_data(), redcap_event_name = "baseline_arm_1")
  schedule <- tibble::tibble(
    record_id = character(), instrument = character(),
    redcap_event_name = character(), repeat_instance = integer()
  )
  plan <- plan_explicit(data, rcon, schedule)
  result <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)

  expect_identical(nrow(result$target_results), 0L)
  expect_identical(nrow(result$summary), 0L)
  expect_identical(nrow(result$missing), 0L)
  expect_identical(nrow(result$details), 0L)
})

test_that("fully gated targets reconcile report evidence", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(run_plan_data(), redcap_event_name = "baseline_arm_1")
  schedule <- tibble::tibble(
    record_id = "absent", instrument = "baseline_form",
    redcap_event_name = "baseline_arm_1", repeat_instance = NA_integer_
  )
  plan <- plan_explicit(data, rcon, schedule)
  result <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)

  expect_identical(result$target_results$event_row_started, "failed")
  expect_identical(result$target_results$instrument_started, "not reached")
  expect_identical(result$target_results$field_complete, "not reached")
  event <- result$summary[result$summary$validation_check == "event-row-started", ]
  expect_identical(event[c("assessed", "passed", "failed")],
                   tibble::tibble(assessed = 1L, passed = 0L, failed = 1L))
  expect_identical(result$missing$validation_check, "event-row-started")
  expect_false(any(result$missing$validation_check %in%
    c("instrument-started", "field-complete")))
})

test_that("mixed target outcomes reconcile statuses summaries missing rows and provenance", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data(record_id = "pass", required_note = "complete"),
    run_plan_data(record_id = "fail", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan, data, rcon,
    ignore_fields = c("record_id", "branch_flag", "checkbox_field", "conditional_note"),
    details = TRUE, progress = FALSE
  )

  legal <- c("passed", "failed", "not applicable", "not reached")
  status_columns <- c(
    "event_row_started", "repeat_instance_row_started",
    "instrument_started", "field_complete"
  )
  expect_true(all(unlist(result$target_results[status_columns], use.names = FALSE) %in% legal))
  expect_identical(
    result$target_results$field_complete[match(c("pass", "fail"), result$target_results$record_id)],
    c("passed", "failed")
  )
  expect_identical(result$target_results$target_source, c("observed", "observed"))

  field_summary <- result$summary[result$summary$validation_check == "field-complete", ]
  expect_identical(
    field_summary[c("assessed", "passed", "failed", "pass_rate", "fail_rate")],
    tibble::tibble(
      assessed = 2L, passed = 1L, failed = 1L,
      pass_rate = 0.5, fail_rate = 0.5
    )
  )
  expect_identical(result$missing$record_id, "fail")
  expect_identical(result$missing$validation_check, "field-complete")
  expect_identical(result$missing$field_name, "required_note")
  expect_false("pass" %in% result$missing$record_id)
  expect_identical(sum(result$target_results$fields_failed), nrow(result$missing))

  expect_type(result$target_results$redcap_event_name, "character")
  expect_type(result$target_results$repeat_instrument, "character")
  expect_type(result$target_results$repeat_instance, "integer")
  expect_true(all(is.na(result$target_results$redcap_event_name)))
  expect_true(all(is.na(result$target_results$repeat_instrument)))
  expect_true(all(is.na(result$target_results$repeat_instance)))
  expect_true(is.na(result$missing$redcap_event_name))
  expect_true(is.na(result$missing$repeat_instrument))
  expect_true(is.na(result$missing$repeat_instance))
})

test_that("all failing targets reconcile exactly without losing structural absence", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data(record_id = "one", required_note = ""),
    run_plan_data(record_id = "two", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan, data, rcon,
    ignore_fields = c("record_id", "branch_flag", "checkbox_field", "conditional_note"),
    details = TRUE, progress = FALSE
  )

  expect_identical(result$target_results$target_source, c("observed", "observed"))
  expect_identical(result$target_results$event_row_started,
                   c("not applicable", "not applicable"))
  expect_identical(result$target_results$repeat_instance_row_started,
                   c("not applicable", "not applicable"))
  expect_identical(result$target_results$instrument_started, c("passed", "passed"))
  expect_identical(result$target_results$field_complete, c("failed", "failed"))
  expect_identical(result$target_results$fields_assessed, c(1L, 1L))
  expect_identical(result$target_results$fields_failed, c(1L, 1L))

  field_summary <- result$summary[result$summary$validation_check == "field-complete", ]
  expect_identical(
    field_summary[c("assessed", "passed", "failed", "pass_rate", "fail_rate")],
    tibble::tibble(
      assessed = 2L, passed = 0L, failed = 2L,
      pass_rate = 0, fail_rate = 1
    )
  )
  expect_setequal(result$missing$record_id, c("one", "two"))
  expect_true(all(result$missing$validation_check == "field-complete"))
  expect_true(all(result$missing$field_name == "required_note"))
  expect_identical(nrow(result$missing), sum(result$target_results$fields_failed))
  expect_true(all(is.na(result$missing$redcap_event_name)))
  expect_true(all(is.na(result$missing$repeat_instrument)))
  expect_true(all(is.na(result$missing$repeat_instance)))
  expect_type(result$missing$redcap_event_name, "character")
  expect_type(result$missing$repeat_instrument, "character")
  expect_type(result$missing$repeat_instance, "integer")
})

test_that("report tables use exact column order", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE, details = TRUE)

  expect_identical(names(result$target_results), c(
    "record_id", "instrument", "redcap_event_name", "repeat_instrument",
    "repeat_instance", "target_source", "event_row_started",
    "repeat_instance_row_started", "instrument_started", "field_complete",
    "fields_assessed", "fields_failed", "field_applicability_reason"
  ))
  expect_identical(names(result$summary), c(
    "redcap_event_name", "instrument", "repeat_instrument", "repeat_instance",
    "validation_level", "validation_check", "status", "reason", "assessed",
    "passed", "failed", "pass_rate", "fail_rate"
  ))
  expect_identical(names(result$missing), c(
    "record_id", "redcap_event_name", "repeat_instrument", "repeat_instance",
    "validation_context", "instrument", "validation_check", "field_name",
    "field_label", "field_type", "branching_logic", "url"
  ))
})

test_that("summary rows preserve validation-stage order", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(
    result$summary$validation_check,
    c(
      "event-row-started",
      "repeat-instance-row-started",
      "instrument-started",
      "field-complete"
    )
  )
})

test_that("a frozen target without a runtime row reconciles summary and missing", {
  rcon <- run_plan_rcon()
  planner_data <- run_plan_data()
  plan <- plan_explicit(
    planner_data,
    rcon,
    run_plan_explicit_schedule("absent")
  )
  result <- run_plan(
    plan,
    planner_data[0, , drop = FALSE],
    rcon,
    details = TRUE,
    progress = FALSE
  )

  expect_identical(
    result$target_results[c("record_id", "target_source")],
    tibble::tibble(record_id = "absent", target_source = "explicit")
  )

  expect_identical(
    result$summary[c(
      "validation_check", "status", "reason",
      "assessed", "passed", "failed"
    )],
    tibble::tibble(
      validation_check = c(
        "event-row-started", "repeat-instance-row-started",
        "instrument-started", "field-complete"
      ),
      status = c(
        "not applicable", "not applicable", "assessed", "assessed"
      ),
      reason = c(
        "not applicable for classic project", "not a repeating target",
        NA_character_, NA_character_
      ),
      assessed = c(0L, 0L, 1L, 0L),
      passed = c(0L, 0L, 0L, 0L),
      failed = c(0L, 0L, 1L, 0L)
    )
  )
  expect_identical(
    result$missing$validation_check,
    "instrument-started"
  )
})

test_that("explicit zero-target reports retain typed empty components", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  schedule <- run_plan_explicit_schedule()[0, ]
  plan <- plan_explicit(data, rcon, schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_equal(nrow(result$summary), 0L)
  expect_equal(nrow(result$missing), 0L)
  expect_type(result$target_results$repeat_instance, "integer")
  expect_type(result$summary$pass_rate, "double")
})

test_that("zero-target reports omit summary and missing rows", {
  classic_rcon <- run_plan_rcon()
  classic_metadata <- classic_rcon$metadata()
  classic_metadata$required_field <- NULL
  classic_rcon$metadata <- function() classic_metadata
  classic_plan <- plan_explicit(
    run_plan_data(),
    classic_rcon,
    run_plan_explicit_schedule()[0, , drop = FALSE]
  )
  classic <- run_plan(
    classic_plan,
    tibble::tibble(record_id = "1"),
    classic_rcon,
    progress = FALSE
  )
  expect_identical(nrow(classic$summary), 0L)
  expect_identical(nrow(classic$missing), 0L)

  longitudinal_rcon <- run_plan_target_scope_rcon()
  longitudinal_plan <- plan_from_data(
    run_plan_target_scope_data(),
    longitudinal_rcon,
    "inactive_form"
  )
  longitudinal <- run_plan(
    longitudinal_plan,
    tibble::tibble(
      record_id = "1",
      redcap_event_name = "baseline_arm_1"
    ),
    longitudinal_rcon,
    progress = FALSE
  )
  expect_identical(nrow(longitudinal$summary), 0L)
  expect_identical(nrow(longitudinal$missing), 0L)
})
